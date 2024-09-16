module type HTTP_CLIENT = sig
  type t

  val get : t -> Uri.t -> string
  val post : t -> Uri.t -> body:string -> string
  val sleep : t -> float -> unit
end

module type S = sig
  type client
  type job
  type image

  val request : client -> job
  val render : job -> image
  val respond : client -> image -> unit
  val vsplit : job -> (job * job) option
  val hsplit : job -> (job * job) option
  val join : image -> image -> image
end

module Make (Client : HTTP_CLIENT) = struct
  open Client

  type net = Client.t
  type client = { net : net; host : Uri.t }
  type job = Protocol.job
  type image = Protocol.response

  let client ?(uri = "localhost:8080") ~username net =
    {
      net;
      host =
        Uri.add_query_param'
          (Uri.of_string ("http://" ^ uri))
          ("username", username);
    }

  let do_request { net; host; _ } =
    let host = Uri.with_path host (Uri.path host ^ "request") in
    let body = Client.get net host in
    let job = Yojson.Safe.from_string body in
    match Protocol.job_of_yojson job with
    | Ok job -> job
    | Error _ -> failwith "invalid json"

  let respond { net; host; _ } render =
    let host = Uri.with_path host (Uri.path host ^ "respond") in
    let body = Yojson.Safe.to_string @@ Protocol.response_to_yojson render in
    let _ = Client.post net host ~body in
    ()

  let rec request client =
    match do_request client with
    | exception _ ->
        sleep client.net 1.0;
        request client
    | v -> v

  let respond client img = try respond client img with _ -> ()

  open Protocol

  let join a b = List.rev_append a b

  let vsplit ({ sub = req; _ } as job) =
    if req.w * req.h < 8 || req.w <= 1 then None
    else
      let w2 = req.w / 2 in
      let left = { req with w = w2 } in
      let right = { req with x = req.x + w2; w = req.w - w2 } in
      Some ({ job with sub = left }, { job with sub = right })

  let hsplit ({ sub = req; _ } as job) =
    if req.w * req.h < 8 || req.h <= 1 then None
    else
      let h2 = req.h / 2 in
      let top = { req with h = h2 } in
      let bottom = { req with y = req.y + h2; h = req.h - h2 } in
      Some ({ job with sub = top }, { job with sub = bottom })

  open Ray_tracer

  let response_of_image seed rect img =
    let result =
      if img.Image.width = 1 && img.Image.height = 1 then
        Image.read_rgb img 0 0 (fun r g b ->
            Printf.sprintf "rgb(%i,%i,%i)" r g b)
      else
        let bytes = ImagePNG.bytes_of_png img in
        let result = Bytes.unsafe_to_string bytes in
        Base64.encode_string result
    in
    [ { Protocol.rect; result_seed = seed; result } ]

  let render job =
    let { nsamples; max_depth; seed; sub = { x; y; w; h } } = job in
    let { Protocol.scene; camera; viewport } =
      Random.init seed;
      Example.final_scene ~image_width:1408 ~ratio:1.0 ~nsamples ~max_depth ()
    in
    let subviewport =
      Camera.create_subviewport ~upper_left:(x, y) ~viewport_width:w
        ~viewport_height:h viewport
    in
    let arr = Ray.rays_to_colors ~progress_bar:true scene camera subviewport in
    let img = Image.create_rgb w h in
    for x = 0 to w - 1 do
      for y = 0 to h - 1 do
        let { Ray_tracer.Color.r; g; b } = arr.(y).(x) in
        let f c = int_of_float (c *. 255.0) in
        let r, g, b = (f r, f g, f b) in
        Image.write_rgb img x y r g b
      done
    done;
    response_of_image seed job.sub img
end

let () =
  (* Try to work around Uri being not thread safe *)
  Uri.of_string "http://[::::1]:8080" |> ignore;
  Uri.of_string "http://localhost:8080/" |> ignore
