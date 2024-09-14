open Graphics

(** 
  Events: 
  {ul 
  {- 'q' : quit the program}
  {- '+' : zoom in}
  {- '-' : zoom out}
  {- 'a' : move left}
  {- 'w' : move up}
  {- 'd' : move right}
  {- 's' : move down}
  }
*)

let current_zoom = ref 1
let prev_ulcorner = ref (0, 0)
let prev_rbcorner = ref (0, 0)

let move_view image width height dir =
  if !current_zoom = 1 then ()
  else
    let new_ulcorner, new_brcorner, image =
      Zoom.move_view ~width ~height ~image ~zoom_value:!current_zoom
        ~ulcorner:!prev_ulcorner ~rbcorner:!prev_rbcorner dir
    in
    prev_ulcorner := new_ulcorner;
    prev_rbcorner := new_brcorner;

    draw_image (make_image image) 0 0

let apply_zoom image w h mouse_pos in_or_out =
  let zoom_value =
    match in_or_out with `In -> !current_zoom * 2 | `Out -> !current_zoom / 2
  in
  if zoom_value <= 1 then begin
    current_zoom := 1;
    draw_image (make_image image) 0 0
  end
  else
    let max_zoom_value = min w h in
    let zoom_value = min zoom_value max_zoom_value in
    if !current_zoom = zoom_value then ()
    else begin
      let prev_iimin, prev_jjmin = !prev_ulcorner in
      let absolute_mouse_pos =
        ( prev_iimin + (fst mouse_pos / !current_zoom),
          prev_jjmin + (snd mouse_pos / !current_zoom) )
      in
      let ulcorner, rbcorner, new_image =
        Zoom.zoom ~image ~zoom_value absolute_mouse_pos (w - 1, h - 1)
      in
      current_zoom := zoom_value;
      prev_ulcorner := ulcorner;
      prev_rbcorner := rbcorner;
      draw_image (new_image |> make_image) 0 0
    end

let char_to_dir = function
  | 'a' -> `Left
  | 'w' -> `Up
  | 'd' -> `Right
  | 's' -> `Down
  | _ -> `Nothing

let loop image =
  let w = Array.length image.(0) in
  let h = Array.length image in
  prev_rbcorner := (w - 1, h - 1);
  loop_at_exit [ Key_pressed ] (fun status ->
      if status.keypressed then
        match status.key with
        | 'q' -> raise Exit
        | '+' ->
            let mouse_pos = (status.mouse_x, status.mouse_y) in
            apply_zoom image w h mouse_pos `In
        | '-' ->
            let mouse_pos = (status.mouse_x, status.mouse_y) in
            apply_zoom image w h mouse_pos `Out
        | 'a' | 'w' | 's' | 'd' ->
            let dir = char_to_dir status.key in
            move_view image w h dir
        | _ -> ())

let loop_exit () =
  loop_at_exit [ Key_pressed ] (fun status ->
      if status.keypressed then
        match status.key with 'q' -> raise Exit | _ -> ())
