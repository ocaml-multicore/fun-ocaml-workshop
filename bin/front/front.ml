module Websocket = Brr_io.Websocket

let size = 1408
let s x = 100.0 *. float x /. float size

let create ~ctx ~status ~color { Protocol.x; y; w; h } msg =
  let position =
    Brr.At.style
      (Jstr.of_string
         (Printf.sprintf
            {|left:%f%%; top:%f%%; width:%f%%; height:%f%%; background-color:%s|}
            (s x) (s y) (s w) (s h) color))
  in
  let class_ = Brr.At.class' (Jstr.of_string status) in
  let el =
    Brr.El.div ~at:[ class_; position ]
      [
        Brr.El.div
          ~at:[ Brr.At.class' (Jstr.of_string "username") ]
          [ Brr.El.txt (Jstr.of_string msg) ];
      ]
  in
  Brr.El.prepend_children ctx [ el ];
  el

let class_image = Jv.get Jv.global "Image"
let new_image () = Jv.new' class_image [||]
let available = ref 1000
let allocated = ref 0
let pending = ref 0
let images = ref []

let alloc_image () =
  incr pending;
  decr available;
  match !images with
  | [] ->
      incr allocated;
      let image = new_image () in
      image
  | image :: rest ->
      images := rest;
      image

let release x =
  incr available;
  decr pending;
  images := x :: !images

let image_load_base64 b64 =
  let module C = Brr_canvas.C2d in
  let image = alloc_image () in
  let blob_url = Jstr.of_string ("data:image/png;base64," ^ b64) in
  Jv.set image "src" (Jv.of_jstr blob_url);
  image

let is_complete image = Jv.to_bool @@ Jv.get image "complete"
let has_anim = ref false
let to_load = Queue.create ()
let to_draw = Queue.create ()

let rec redraw ~canvas _ =
  has_anim := false;
  while !available > 0 && Queue.length to_load > 0 do
    match Queue.pop to_load with
    | exception Queue.Empty -> ()
    | ((position : Protocol.sub), img) as todo -> (
        match img with
        | b64 when !available > 0 ->
            let image_src = image_load_base64 b64 in
            Queue.push (position, image_src) to_draw
        | _ -> Queue.push todo to_load)
  done;
  for _ = 1 to 1_000 do
    match Queue.pop to_draw with
    | exception Queue.Empty -> ()
    | (position : Protocol.sub), image when is_complete image ->
        let img = Brr_canvas.C2d.image_src_of_jv image in
        Brr_canvas.C2d.draw_image canvas img ~x:(float position.x)
          ~y:(float position.y);
        release image
    | todo -> Queue.push todo to_draw
  done;
  if Queue.length to_draw > 0 || Queue.length to_load > 0 then
    refresh ~canvas ()

and refresh ~canvas () =
  if !has_anim then ()
  else (
    has_anim := true;
    let _ = Brr.G.request_animation_frame (redraw ~canvas) in
    ())

let fresh ctx canvas =
  let children = Brr.El.children ctx in
  List.iter (fun e -> Brr.El.remove e) children;
  Brr.El.append_children ctx [ canvas ];
  let canvas = Brr_canvas.Canvas.of_el canvas in
  let canvas_ctx = Brr_canvas.C2d.get_context canvas in
  Brr_canvas.Canvas.set_w canvas 1408;
  Brr_canvas.Canvas.set_h canvas 1408;
  Brr_canvas.C2d.reset_transform canvas_ctx

let new_message ~ctx ~canvas ~canvas_ctx = function
  | Protocol.Fresh -> fresh ctx canvas
  | Update { username; color; position; status } -> (
      match status with
      | Start ->
          let el = create ~ctx ~status:"start" ~color position username in
          Brr.El.append_children el
            [ Brr.El.div ~at:[ Brr.At.class' (Jstr.of_string "loader") ] [] ];
          ()
      | Resolved str_ ->
          if position.w = 1 && position.h = 1 then (
            Brr_canvas.C2d.set_fill_style canvas_ctx
              (Brr_canvas.C2d.color (Jstr.of_string str_));
            Brr_canvas.C2d.fill_rect canvas_ctx ~x:(float position.x)
              ~y:(float position.y) ~w:1.0 ~h:1.0)
          else (
            Queue.push (position, str_) to_load;
            refresh ~canvas:canvas_ctx ()))

let new_message ~ctx ~canvas ~canvas_ctx msg =
  let msg = Brr.Ev.as_type msg in
  let msg = Brr_io.Message.Ev.data msg in
  let msg = Jstr.to_string msg in
  new_message ~ctx ~canvas ~canvas_ctx (Protocol.of_string msg)

let main () =
  print_endline "yo!";
  let ctx =
    match Brr.El.find_first_by_selector (Jstr.of_string "#image") with
    | None -> failwith "no image element"
    | Some ctx -> ctx
  in
  let canvas = Brr.El.canvas [] in
  Brr.El.append_children ctx [ canvas ];
  let canvas' = Brr_canvas.Canvas.of_el canvas in
  let canvas_ctx = Brr_canvas.C2d.get_context canvas' in
  Brr_canvas.Canvas.set_w canvas' 1408;
  Brr_canvas.Canvas.set_h canvas' 1408;
  Brr_canvas.C2d.reset_transform canvas_ctx;

  let ws = Websocket.create (Jstr.of_string "ws://localhost:8080/watch") in
  let _listener =
    Brr.Ev.listen Brr_io.Message.Ev.message
      (new_message ~ctx ~canvas ~canvas_ctx)
      (Websocket.as_target ws)
  in
  ()

let main () =
  let open Brr in
  Fut.bind (Ev.next Ev.load (Window.as_target G.window)) @@ fun _ev ->
  main ();
  Fut.return ()

let () = ignore (main ())
