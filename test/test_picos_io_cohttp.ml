module Actor = Actor_picos_io_cohttp

let username = Sys.argv.(1)
let uri = try Some Sys.argv.(2) with _ -> None

let () =
  Picos_mux_fifo.run @@ fun () ->
  let client = Actor.client ?uri ~username () in
  while true do
    let sub = Actor.request client in
    let img = Actor.render sub in
    Actor.respond client sub img
  done
