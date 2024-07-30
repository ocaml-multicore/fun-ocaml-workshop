let[@inline] distance x y z = sqrt ((x *. x) +. (y *. y) +. (z *. z))
let[@linline] distance2 x y z = (x *. x) +. (y *. y) +. (z *. z)
let[@inline] degrees_to_radians degrees = degrees *. Float.pi /. 180.0

let[@inline] print_progress height jj =
  if jj mod 100 = 0 then
    Format.printf "\rScanlines remaining: %d @." (height - jj)

let[@inline] map2Dij ?(progress_bar = false) f matrix =
  Array.mapi
    (fun jj row ->
      if progress_bar then print_progress (Array.length matrix) jj;
      Array.mapi (fun ii x -> f ii jj x) row)
    matrix

let[@inline] map2D ?(progress_bar = false) f matrix =
  Array.mapi
    (fun jj row ->
      if progress_bar then print_progress (Array.length matrix) jj;
      Array.map (fun x -> f x) row)
    matrix

let[@inline] close_enough_float ?(epsilon = Float.epsilon) a b =
  Float.abs (a -. b) <= epsilon
