open Lwt.Syntax
module H = Tyxml_html

let timeout = 1.0
let cell_size = 128
let nsamples = 20
let max_depth = 10
let max_pending = cell_size * cell_size * 3
let width = 1408
let height = 1408
let () = Random.self_init ()
let string_of_html html = Format.asprintf "%a" (H.pp ()) html

let send ws msg =
  Lwt.catch
    (fun () -> Dream.send ws (Protocol.to_string msg))
    (fun _ -> Lwt.return_unit)

let knuth_shuffle a =
  let n = Array.length a in
  for i = n - 1 downto 1 do
    let k = Random.int (i + 1) in
    let x = a.(k) in
    a.(k) <- a.(i);
    a.(i) <- x
  done

let clients = ref []

type state = {
  current_task : Protocol.task;
  current_seed : int;
  todos : Protocol.sub Queue.t;
  mutable remaining : int;
  is_done : bool array array;
}

let make_state () =
  let () =
    Lwt.dont_wait
      (fun () -> Lwt_list.iter_p (fun ws -> send ws Protocol.Fresh) !clients)
      (fun _ -> ())
  in
  let arr =
    Array.concat
      (List.init (width / cell_size) (fun x ->
           Array.init (height / cell_size) (fun y ->
               let x = x * cell_size in
               let y = y * cell_size in
               { Protocol.x; y; w = cell_size; h = cell_size })))
  in
  knuth_shuffle arr;
  let seed = Random.int 1_000_000_000 in
  let todos = Queue.create () in
  Array.iter (fun x -> Queue.add x todos) arr;
  {
    current_task =
      Example_scene.final_scene ~image_width:width ~ratio:1.0 ~nsamples
        ~max_depth ();
    current_seed = seed;
    todos;
    remaining = width * height;
    is_done = Array.make_matrix width height false;
  }

let state = ref (make_state ())

let is_done (rect : Protocol.sub) =
  let exception Todo in
  let state = !state in
  let img = state.is_done in
  try
    for y = rect.y to rect.y + rect.h - 1 do
      for x = rect.x to rect.x + rect.w - 1 do
        if not img.(y).(x) then raise Todo
      done
    done;
    true
  with Todo -> false

let rec pop () =
  let state = !state in
  match Queue.pop state.todos with
  | exception Queue.Empty -> None
  | sub_image when is_done sub_image -> pop ()
  | sub_image ->
      Queue.push sub_image state.todos;
      Some sub_image

let mark_done state (rect : Protocol.sub) =
  let updated = ref 0 in
  let img = state.is_done in
  for y = rect.y to rect.y + rect.h - 1 do
    for x = rect.x to rect.x + rect.w - 1 do
      if not img.(y).(x) then (
        incr updated;
        img.(y).(x) <- true;
        state.remaining <- state.remaining - 1)
    done
  done;
  !updated

type timestamp = float

type user = {
  color : string;
  mutable pending : (timestamp * int) list;
  has_done : (int * int, unit) Hashtbl.t;
}

let user_mark_done user { Protocol.x; y; w; h } =
  let count = ref 0 in
  for x = x to x + w - 1 do
    for y = y to y + h - 1 do
      if not (Hashtbl.mem user.has_done (x, y)) then (
        Hashtbl.replace user.has_done (x, y) ();
        incr count)
    done
  done;
  user.pending <- (Unix.gettimeofday (), - !count) :: user.pending

let pending_count user =
  let now = Unix.gettimeofday () in
  let pending = List.filter (fun (t, _) -> now -. t < timeout) user.pending in
  let sum = List.fold_left (fun acc (_, c) -> acc + c) 0 pending in
  user.pending <- pending;
  sum

let users = Hashtbl.create 16

let get_user query =
  let username =
    match Dream.query query "username" with None -> "" | Some u -> u
  in
  let user =
    try Hashtbl.find users username
    with Not_found ->
      let hue = mod_float (1.618 *. float (Hashtbl.length users)) 1.0 in
      let hue = 360.0 *. hue in
      let color = Color.of_hsl hue 0.5 0.5 in
      let color = Color.to_css_rgba color in
      let user = { color; pending = []; has_done = Hashtbl.create 0 } in
      Hashtbl.replace users username user;
      user
  in
  (username, user)

let interface = try Some Sys.argv.(1) with _ -> None

let () =
  Dream.run ~port:8080 ?interface
  @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (fun _ ->
             Dream.html
             @@ string_of_html
                  H.(
                    html
                      (head
                         (title (txt "Fun OCaml 2024 - Multicore workshop"))
                         [
                           script ~a:[ a_src "/front.js" ] (txt "");
                           link ~rel:[ `Stylesheet ] ~href:"/style.css" ();
                         ])
                      (body
                         [
                           h1 [ txt "Fun OCaml 2024 - Multicore workshop" ];
                           div ~a:[ a_id "image" ] [];
                         ])));
         Dream.get "/request" (fun query ->
             let username, user = get_user query in
             if pending_count user >= max_pending then
               Lwt.return
                 (Dream.response ~headers:[ ("Content-Type", "text/json") ] "")
             else
               match pop () with
               | None ->
                   Lwt.return
                     (Dream.response
                        ~headers:[ ("Content-Type", "text/json") ]
                        "")
               | Some sub ->
                   let job =
                     {
                       Protocol.task = !state.current_task;
                       seed = !state.current_seed;
                       sub;
                     }
                   in
                   user.pending <-
                     (Unix.gettimeofday (), sub.w * sub.h) :: user.pending;
                   let () =
                     Lwt.dont_wait
                       (fun () ->
                         Lwt_list.iter_p
                           (fun ws ->
                             send ws
                             @@ Protocol.Update
                                  {
                                    username;
                                    color = user.color;
                                    position = sub;
                                    status = Start;
                                  })
                           !clients)
                       (fun _ -> ())
                   in
                   Lwt.return
                     (Dream.response
                        ~headers:[ ("Content-Type", "text/json") ]
                        (Yojson.Safe.to_string @@ Protocol.job_to_yojson job)));
         Dream.post "/respond" (fun query ->
             let username, user = get_user query in
             let+ body = Dream.body query in
             let lst =
               Result.get_ok
               @@ Protocol.response_of_yojson (Yojson.Safe.from_string body)
             in
             let () =
               lst
               |> List.iter (fun { Protocol.rect; result_seed; result } ->
                      if result_seed = !state.current_seed then (
                        user_mark_done user rect;
                        if mark_done !state rect > 0 then (
                          Lwt.dont_wait
                            (fun () ->
                              Lwt_list.iter_p
                                (fun ws ->
                                  send ws
                                  @@ Protocol.Update
                                       {
                                         username;
                                         color = user.color;
                                         position = rect;
                                         status = Resolved result;
                                       })
                                !clients)
                            (fun _ -> ());
                          if !state.remaining <= 0 then (
                            Queue.clear !state.todos;
                            Lwt.dont_wait
                              (fun () ->
                                let+ () = Lwt_unix.sleep 3.0 in
                                Hashtbl.clear users;
                                state := make_state ())
                              (fun _ -> ())))))
             in
             Dream.response ~headers:[ ("Content-Type", "text/json") ] "");
         Dream.get "/style.css" (fun _ ->
             Lwt.return
             @@ Dream.response
                  ~headers:[ ("Content-Type", "text/css") ]
                  [%blob "bin/server/style.css"]);
         Dream.get "/front.js" (fun _ ->
             Lwt.return
             @@ Dream.response
                  ~headers:[ ("Content-Type", "text/javascript") ]
                  [%blob "bin/front/front.bc.js"]);
         Dream.get "/watch" (fun _ ->
             Dream.websocket @@ fun ws ->
             clients := ws :: !clients;
             let rec loop () =
               let* msg = Dream.receive ws in
               match msg with
               | None -> Dream.close_websocket ws
               | Some _ -> loop ()
             in
             loop ());
       ]
