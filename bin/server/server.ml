open Lwt.Syntax
module H = Tyxml_html

let cell_size = 64
let width = 1408
let height = 1408

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

type state = Protocol.sub Queue.t

let state : state = Queue.create ()

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
  Array.iter (fun x -> Queue.add x state) arr

let rec pop () =
  match Queue.pop state with
  | exception Queue.Empty ->
      make_state ();
      pop ()
  | sub_image -> sub_image

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
             let sub_image = pop () in
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
                              position = sub_image;
                              status = Start;
                            })
                     !clients)
                 (fun _ -> ())
             in
             Lwt.return
               (Dream.response
                  ~headers:[ ("Content-Type", "text/json") ]
                  (Yojson.Safe.to_string @@ Protocol.sub_to_yojson sub_image)));
         Dream.post "/respond" (fun query ->
             let username, color = get_user query in
             let* body = Dream.body query in
             let { Protocol.sub; result } =
               Result.get_ok
               @@ Protocol.response_of_yojson (Yojson.Safe.from_string body)
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
                              status = Resolved result;
                            })
                     !clients)
                 (fun _ -> ())
             in
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
