open Lwt.Syntax
module H = Tyxml_html

let cell_size = 128
let width = 1408
let height = 1408
let ratio = float_of_int width /. float_of_int height
(* *)

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
  current : Ray_tracer.Scene.scene;
  camera : Ray_tracer.Camera.camera;
  viewport : Ray_tracer.Camera.viewport;
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
  let todos = Queue.create () in
  let camera =
    Ray_tracer.Camera.create ~image_width:width ~ratio
      ~camera_center:(Ray_tracer.Pos.create 0. 0. 0.)
      ~focal_length:1. ()
  in
  let viewport = Ray_tracer.Camera.create_viewport ~viewport_height:2. camera in
  Array.iter (fun x -> Queue.add x todos) arr;
  {
    current = Scenes.random_scene ();
    camera;
    viewport;
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

let mark_done (rect : Protocol.sub) =
  let updated = ref false in
  let state = !state in
  let img = state.is_done in
  for y = rect.y to rect.y + rect.h - 1 do
    for x = rect.x to rect.x + rect.w - 1 do
      if not img.(y).(x) then (
        updated := true;
        img.(y).(x) <- true;
        state.remaining <- state.remaining - 1)
    done
  done;
  !updated

let users = Hashtbl.create 16

let get_user query =
  let username =
    match Dream.query query "username" with None -> "" | Some u -> u
  in
  let color =
    try Hashtbl.find users username
    with Not_found ->
      let hue = mod_float (1.618 *. float (Hashtbl.length users)) 1.0 in
      let hue = 360.0 *. hue in
      let c = Color.of_hsl hue 0.5 0.5 in
      let c = Color.to_css_rgba c in
      Hashtbl.replace users username c;
      c
  in
  (username, color)

let () =
  Dream.run @@ Dream.logger
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
             let username, color = get_user query in
             match pop () with
             | None ->
                 Lwt.return
                   (Dream.response
                      ~headers:[ ("Content-Type", "text/json") ]
                      "")
             | Some sub ->
                 let job =
                   {
                     Protocol.task =
                       {
                         scene = !state.current;
                         camera_center =
                           Ray_tracer.Camera.camera_center !state.camera;
                         viewport = !state.viewport;
                       };
                     sub;
                   }
                 in
                 let () =
                   Lwt.dont_wait
                     (fun () ->
                       Lwt_list.iter_p
                         (fun ws ->
                           send ws
                           @@ Protocol.Update
                                {
                                  username;
                                  color;
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
             let username, color = get_user query in
             let* body = Dream.body query in
             let { Protocol.rect; result } =
               Result.get_ok
               @@ Protocol.response_of_yojson (Yojson.Safe.from_string body)
             in
             if mark_done rect then (
               Lwt.dont_wait
                 (fun () ->
                   Lwt_list.iter_p
                     (fun ws ->
                       send ws
                       @@ Protocol.Update
                            {
                              username;
                              color;
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
                     state := make_state ())
                   (fun _ -> ())));
             Lwt.return
             @@ Dream.response ~headers:[ ("Content-Type", "text/json") ] "");
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
