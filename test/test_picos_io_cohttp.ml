open Picos_std_structured
module Actor = Actor_picos_io_cohttp

let n_domains = 4
let username = Sys.argv.(1)

let () =
  Picos_mux_multififo.run_on ~n_domains @@ fun () ->
  let client = Actor.client ~username in
  Run.all @@ List.init n_domains
  @@ fun _ () ->
  while true do
    let sub = Actor.request client in
    let img = Actor.render sub in
    Actor.respond client sub img
  done
