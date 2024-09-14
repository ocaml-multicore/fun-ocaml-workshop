open Graphics

let do_image image x y = draw_image (make_image image) x y

let example ~number =
  let ratio = 16. /. 9. in
  let image_width = 1400 in
  let image_height = int_of_float (Float.of_int image_width /. ratio) in
  match number with
  | 1 ->
      open_graph (Printf.sprintf " %dx%d" image_width image_height);

      let image = Example.example5 ~progress_bar:true () in
      do_image image 0 0;
      Event.loop image
  | 2 ->
      open_graph (Printf.sprintf " %dx%d" image_width image_height);

      let image = Example.example6 ~progress_bar:true () in
      do_image image 0 0;
      Event.loop image
  | 3 ->
      open_graph (Printf.sprintf " %dx%d" image_width image_height);

      let image = Example.example7 ~progress_bar:true ~image_width ~ratio () in
      do_image image 0 0;
      Event.loop image
  | 4 ->
      open_graph (Printf.sprintf " %dx%d" image_width image_height);

      let image =
        Example.final_scene ~progress_bar:true ~image_width ~ratio ()
      in
      do_image image 0 0;
      Event.loop image
  | 5 ->
      let n = 25 in
      let image_width = 64 * n in
      let image_height = 64 * int_of_float (float_of_int n *. 9. /. 16.) in
      let ratio = Float.of_int image_width /. Float.of_int image_height in
      open_graph (Printf.sprintf " %dx%d" image_width image_height);

      let res_queue = Saturn_lockfree.Single_consumer_queue.create () in

      let subviewport_width = 16 in
      let subviewport_length = 16 in
      let divu = image_width / subviewport_width in
      let divv = image_width / subviewport_length in

      assert (
        image_width mod subviewport_width = 0
        && image_width mod subviewport_length = 0);

      let workers =
        Example.final_scene_par ~image_width ~ratio ~subviewport_length
          ~subviewport_width res_queue ()
      in

      let count = ref 0 in
      while !count < divu * divv do
        match Saturn_lockfree.Single_consumer_queue.pop_opt res_queue with
        | None -> Unix.sleep 1
        | Some ((x, y), colors) ->
            Format.printf "Got %d %d\n%!" x y;
            do_image colors x (image_height - y);
            incr count
      done;

      Array.iter Domain.join workers;
      Event.loop_exit ()
  | _ -> failwith "Bad example number"

let main number =
  Random.self_init ();
  example ~number

(* Command line parsing *)
let usage_msg = "dune exec ./main.exe -- -number [example_number]"
let max_number = ref 5
let example_number = max_number
let speclist = [ ("-number", Arg.Set_int example_number, "Example number") ]
let anon_fun _ = ()

let () =
  Arg.parse speclist anon_fun usage_msg;
  main !example_number
