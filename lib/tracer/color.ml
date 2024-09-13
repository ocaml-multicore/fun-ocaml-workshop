type t = { r : float; g : float; b : float } [@@deriving yojson]

let rgb r g b = { r; g; b }
let white = rgb 1. 1. 1.
let black = rgb 0. 0. 0.

let add rgb1 rgb2 =
  { r = rgb1.r +. rgb2.r; g = rgb1.g +. rgb2.g; b = rgb1.b +. rgb2.b }

let clamp rgb =
  {
    r = min 1. (max 0. rgb.r);
    g = min 1. (max 0. rgb.g);
    b = min 1. (max 0. rgb.b);
  }

let avg n rgb =
  assert (n > 0);
  let nf = Float.of_int n in
  assert (rgb.r <= nf && rgb.g <= nf && rgb.b <= nf);
  { r = rgb.r /. nf; g = rgb.g /. nf; b = rgb.b /. nf }

let scale f rgb = { r = f *. rgb.r; g = f *. rgb.g; b = f *. rgb.b }
let linear_to_gamma linear = if linear > 0.0 then sqrt linear else 0.

let linear_to_gamma rgb =
  {
    r = linear_to_gamma rgb.r;
    g = linear_to_gamma rgb.g;
    b = linear_to_gamma rgb.b;
  }

let mul rgb1 rgb2 =
  { r = rgb1.r *. rgb2.r; g = rgb1.g *. rgb2.g; b = rgb1.b *. rgb2.b }

let random_float min max = Random.float (max -. min) +. min

let random ?(min = 0.) ?(max = 1.) () =
  rgb (random_float min max) (random_float min max) (random_float min max)
