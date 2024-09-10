include Actor_shared.Make (struct
  type t = unit

  let to_string (_, body) = Cohttp.Body.to_string body
  let get () uri = to_string (Picos_io_cohttp.Client.get uri)

  let post () uri ~body =
    let body = Cohttp.Body.of_string body in
    to_string (Picos_io_cohttp.Client.post ~body uri)

  let sleep () seconds = Picos_std_structured.Control.sleep ~seconds
end)

let client = client ()
