include Actor_shared.Make (struct
  type t =
    < clock : float Eio.Time.clock_ty Eio.Time.clock
    ; net : Cohttp_eio.Client.t >

  let to_string (_, body) = Eio.Flow.read_all body

  let get t uri =
    Eio.Switch.run @@ fun sw -> to_string (Cohttp_eio.Client.get ~sw t#net uri)

  let post t uri ~body =
    Eio.Switch.run @@ fun sw ->
    let body = Cohttp_eio.Body.of_string body in
    to_string (Cohttp_eio.Client.post ~sw t#net ~body uri)

  let sleep t seconds = Eio.Time.sleep t#clock seconds
end)

let client env ~username =
  let net = Cohttp_eio.Client.make ~https:None env#net in
  let t =
    object
      method clock = env#clock
      method net = net
    end
  in
  client t ~username
