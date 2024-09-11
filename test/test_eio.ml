open Eio
module Actor = Actor_eio

let username = Sys.argv.(1)

let () =
  Eio_main.run @@ fun env ->
  let client = Actor.client env ~username in
  Fiber.all
  @@ List.init (Domain.recommended_domain_count ())
  @@ fun _ () ->
  Domain_manager.run (Stdenv.domain_mgr env) @@ fun () ->
  while true do
    let sub = Actor.request client in
    let img = Actor.render sub in
    Actor.respond client sub img
  done
