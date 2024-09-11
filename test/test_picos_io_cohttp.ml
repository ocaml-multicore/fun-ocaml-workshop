open Picos_std_structured
module Actor = Actor_picos_io_cohttp

let n_domains = 4
let username = Sys.argv.(1)

let main () =
  Flock.join_after @@ fun () ->
  for _ = 1 to n_domains do
    Flock.fork @@ fun () ->
    let client = Actor.client ~username in

    while true do
      let sub = Actor.request client in
      let img = Actor.render sub in
      Actor.respond client sub img;

      Control.sleep ~seconds:(0.2 +. Random.float 0.03)
    done
  done

let () = Picos_mux_multififo.run_on ~n_domains main
