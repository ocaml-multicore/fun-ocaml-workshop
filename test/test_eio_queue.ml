module Actor = Actor_eio
module Queue = Saturn.Queue

let username = Sys.argv.(1)
let uri = try Some Sys.argv.(2) with _ -> None

let rec split sub n =
  if n = 0 then [ sub ]
  else
    match if Random.bool () then Actor.hsplit sub else Actor.vsplit sub with
    | Some (left, right) ->
        let n = n - 1 in
        split left n @ split right n
    | None -> [ sub ]

let () =
  Eio_main.run @@ fun env ->
  let client = Actor.client ?uri ~username env in
  let queue = Queue.create () in
  while true do
    match Queue.pop_opt queue with
    | Some job ->
        let img = Actor.render job in
        Actor.respond client img
    | None ->
        let job = Actor.request client in
        let parts = split job 3 in
        List.iter (Queue.push queue) parts
  done
