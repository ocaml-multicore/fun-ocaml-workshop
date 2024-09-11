open Eio
module Actor = Actor_eio
module Queue = Saturn.Queue

let rec split sub n =
  if n = 0 then [ sub ]
  else
    let left, right =
      if Random.bool () then Actor.hsplit sub else Actor.vsplit sub
    in
    let n = n - 1 in
    split left n @ split right n

let username = Sys.argv.(1)

let () =
  Eio_main.run @@ fun env ->
  let client = Actor.client env ~username in
  let queue = Queue.create () in

  Fiber.all
  @@ List.init (Domain.recommended_domain_count ())
  @@ fun _ () ->
  Domain_manager.run (Stdenv.domain_mgr env) @@ fun () ->
  while true do
    match Queue.pop_opt queue with
    | Some job ->
        let img = Actor.render job in
        Actor.respond client job img
    | None ->
        let job = Actor.request client in
        let parts = split job 3 in
        List.iter (Queue.push queue) parts
  done
