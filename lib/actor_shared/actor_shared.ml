module type HTTP_CLIENT = sig
  type t

  val get : t -> Uri.t -> string
  val post : t -> Uri.t -> body:string -> string
  val sleep : t -> float -> unit
end

module type S = sig
  type client
  type job = Protocol.sub = { x : int; y : int; w : int; h : int }

  val request : client -> job
  val respond : client -> job -> Image.image -> unit
  val vsplit : job -> job * job
  val hsplit : job -> job * job
end

module Make (Client : HTTP_CLIENT) = struct
  open Client

  type net = Client.t
  type client = { net : net; host : Uri.t }
  type job = Protocol.sub = { x : int; y : int; w : int; h : int }

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
    let sub = Yojson.Safe.from_string body in
    match Protocol.sub_of_yojson sub with
    | Ok sub -> sub
    | Error _ -> failwith "invalid json"

  let respond { net; host; _ } req img =
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
      Yojson.Safe.to_string @@ Protocol.response_to_yojson { sub = req; result }
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

  let vsplit (req : Protocol.sub) =
    let w2 = req.w / 2 in
    let left = { req with w = w2 } in
    let right = { req with x = req.x + w2; w = req.w - w2 } in
    (left, right)

  let hsplit (req : Protocol.sub) =
    let h2 = req.h / 2 in
    let top = { req with h = h2 } in
    let bottom = { req with y = req.y + h2; h = req.h - h2 } in
    (top, bottom)
end
