open Graphics

let example ~number =
  let ratio = 16. /. 9. in
  let image_width = 1600 in
  let image_height = Float.of_int image_width /. ratio |> Float.to_int in
  open_graph (Printf.sprintf " %dx%d" image_width image_height);
  match number with
  | 1 -> Example.example5 ~progress_bar:true ()
  | 2 -> Example.example6 ~progress_bar:true ()
  | 3 -> Example.example7 ~progress_bar:true ~image_width ~ratio ()
  | 4 -> Example.final_scene ~progress_bar:true ~image_width ~ratio ()
  | _ -> failwith "Bad example number"

let main number =
  Random.self_init ();
  let image = example ~number in
  draw_image (make_image image) 0 0;
  Event.loop image

(* Command line parsing *)
let usage_msg = "dune exec ./main.exe -- -number [example_number]"
let max_number = ref 4
let example_number = max_number
let speclist = [ ("-number", Arg.Set_int example_number, "Example number") ]
let anon_fun _ = ()

let () =
  Arg.parse speclist anon_fun usage_msg;
  main !example_number
