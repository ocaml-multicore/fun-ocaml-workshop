module type HTTP_CLIENT = sig
  type t

  val get : t -> Uri.t -> string
  val post : t -> Uri.t -> body:string -> string
  val sleep : t -> float -> unit
end

module type S = sig
  type client
  type job

  val request : client -> job
  val respond : client -> job -> Image.image -> unit
  val render : job -> Image.image
  val vsplit : job -> job * job
  val hsplit : job -> job * job
end

module Make (Client : HTTP_CLIENT) = struct
  open Client

  type net = Client.t
  type client = { net : net; host : Uri.t }
  type job = Protocol.job

  let client net ~username =
    {
      net;
      host =
        Uri.add_query_param'
          (Uri.of_string "http://localhost:8080/")
          ("username", username);
    }

  let do_request { net; host; _ } =
    let host = Uri.with_path host (Uri.path host ^ "request") in
    let body = Client.get net host in
    let job = Yojson.Safe.from_string body in
    match Protocol.job_of_yojson job with
    | Ok job -> job
    | Error _ -> failwith "invalid json"

  let respond { net; host; _ } { Protocol.sub = req; _ } img =
    let result =
      if img.Image.width = 1 && img.Image.height = 1 then
        Image.read_rgb img 0 0 (fun r g b ->
            Printf.sprintf "rgb(%i,%i,%i)" r g b)
      else
        let bytes = ImagePNG.bytes_of_png img in
        let result = Bytes.unsafe_to_string bytes in
        Base64.encode_string result
    in
    let host = Uri.with_path host (Uri.path host ^ "respond") in
    let body =
      Yojson.Safe.to_string
      @@ Protocol.response_to_yojson { rect = req; result }
    in
    let _ = Client.post net host ~body in
    ()

  let rec request client =
    match do_request client with
    | exception _ ->
        sleep client.net 1.0;
        request client
    | v -> v

  let respond client req img = try respond client req img with _ -> ()

  open Protocol

  let vsplit ({ sub = req; _ } as job) =
    let w2 = req.w / 2 in
    let left = { req with w = w2 } in
    let right = { req with x = req.x + w2; w = req.w - w2 } in
    ({ job with sub = left }, { job with sub = right })

  let hsplit ({ sub = req; _ } as job) =
    let h2 = req.h / 2 in
    let top = { req with h = h2 } in
    let bottom = { req with y = req.y + h2; h = req.h - h2 } in
    ({ job with sub = top }, { job with sub = bottom })

  open Ray_tracer

  let example_scene =
    let open Scene in
    let objs =
      [
        {
          form = sphere (Pos.create 0. 0. (-1.)) 0.5;
          material = Material.create_lambertian (Color.rgb 0.5 0.5 0.5);
        };
        {
          form = sphere (Pos.create 0. 0.5 (-1.)) 0.3;
          material = Material.create_lambertian (Color.rgb 0.5 0. 0.);
        };
      ]
      |> List.map to_obj_t
    in
    let scale1 = build_scale 0.5 in
    let transla1 = build_translation (Vect.create 0.5 0. 0.) in
    let scale2 = build_scale 0.25 in
    let transla2 = build_translation (Vect.create (-0.5) 0. 0.) in
    let rec scene =
      [
        with_transfo ~transfo:[ scale1; transla1 ] (of_list objs);
        Transfo
          {
            transfo = scale2;
            obj = Transfo { transfo = transla2; obj = Compo scene };
          };
      ]
    in
    scene

  let render job =
    let { Protocol.task = { camera_center; viewport; _ }; sub = { x; y; w; h } }
        =
      job
    in

    let subviewport =
      Camera.create_subviewport ~upper_left:(x, y) ~viewport_width:w
        ~viewport_height:h viewport
    in
    let arr =
      Ray.rays_to_colors ~progress_bar:true ~nsamples:10 ~max_depth:10
        example_scene camera_center subviewport
    in
    let img = Image.create_rgb w h in
    for x = 0 to w - 1 do
      for y = 0 to h - 1 do
        let { Ray_tracer.Color.r; g; b } = arr.(y).(x) in
        let f c = int_of_float (c *. 255.0) in
        let r, g, b = (f r, f g, f b) in
        Image.write_rgb img x y r g b
      done
    done;
    img
end

let () =
  (* Try to work around Uri being not thread safe *)
  Uri.of_string "http://[::::1]:8080" |> ignore;
  Uri.of_string "http://localhost:8080/" |> ignore
