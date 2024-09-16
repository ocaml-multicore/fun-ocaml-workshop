module Actor = Actor_eio

let username = Sys.argv.(1)

let () =
  Eio_main.run @@ fun env ->
  let client = Actor.client env ~username in
  while true do
    let sub = Actor.request client in
    let img = Actor.render sub in
    Actor.respond client sub img
  done
