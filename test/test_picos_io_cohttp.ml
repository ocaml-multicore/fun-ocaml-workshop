open Picos_std_structured
module Actor = Actor_picos_io_cohttp

let quality = 255

let iteration_count x0 y0 =
  let rec iter' x y n =
    if n = quality then 255
    else if (x *. x) +. (y *. y) > 4.0 then n * 256 / quality
    else iter' ((x *. x) -. (y *. y) +. x0) ((2.0 *. x *. y) +. y0) (succ n)
  in
  iter' 0.0 0.0 0

let iteration_count x0 y0 =
  iteration_count ((4.0 *. x0) -. 2.0) ((4.0 *. y0) -. 2.0)

let color x y =
  let n = iteration_count (float x /. 1408.0) (float y /. 1408.0) in
  (n, n, n)

let myimage x' y' w h =
  let img = Image.create_rgb w h in
  for x = 0 to w - 1 do
    for y = 0 to h - 1 do
      let r, g, b = color (x + x') (y + y') in
      Image.write_rgb img x y r g b
    done
  done;
  img

(* let split sub _ = [sub] *)
(* let rec split (sub: Protocol.sub) n = *)
(*   if sub.w = 1 && sub.h = 1 then [sub] *)
(*   else if sub.w = 0 || sub.h = 0 then [] *)
(*   else let left, right = if sub.w > sub.h then Actor.vsplit sub else Actor.hsplit sub in *)
(*        let n = n - 1 in *)
(*        split left n @ split right n *)

let rec split (sub : Protocol.sub) n =
  if n = 0 then [ sub ]
  else
    let left, right =
      if sub.w > sub.h then Actor.vsplit sub else Actor.hsplit sub
    in
    let n = n - 1 in
    split left n @ split right n

let username = Sys.argv.(1)

let main () =
  let client = Actor.client ~username in

  while true do
    let sub = Actor.request client in
    let parts = split sub 3 in
    List.iter
      (fun sub ->
        let open Protocol in
        let img = myimage sub.x sub.y sub.w sub.h in
        Control.sleep ~seconds:(0.2 +. Random.float 0.03);
        Actor.respond client sub img)
      parts
  done

let () = Picos_mux_multififo.run_on ~n_domains:1 main
