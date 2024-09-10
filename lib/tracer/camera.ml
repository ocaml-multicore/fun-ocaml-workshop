type camera = {
  camera_center : Pos.t;
  ux : Vect.t;
  uy : Vect.t;
  uz : Vect.t;
  image_width : int;
  image_height : int;
  focal_length : float;
}
[@@deriving yojson]

let camera_center camera = camera.camera_center
let image_width camera = camera.image_width
let image_height camera = camera.image_height

let create ?(ux = Vect.create 1. 0. 0.) ?(uz = Vect.create 0. 0. 1.)
    ?(ratio = 16. /. 9.) ?(image_width = 400) ~camera_center ~focal_length () =
  let image_height =
    int_of_float (float_of_int image_width /. ratio) |> max 1
  in
  let ux = Vect.unit_vector ux in
  let uz = Vect.unit_vector uz in
  let uy = Vect.cross uz ux in
  { ux; uy; uz; camera_center; image_width; image_height; focal_length }

let default = create ~camera_center:(Pos.create 0. 0. 0.) ~focal_length:1. ()

type viewport = {
  upper_left : Pos.t;
  pixel_delta_u : Vect.t;
  pixel_delta_v : Vect.t;
  viewport_width : int;
  viewport_height : int;
  pixel_size : float;
}
[@@deriving yojson]

let create_viewport ?(viewport_height = 2.) camera =
  let pixel_size = viewport_height /. float_of_int camera.image_height in
  let viewport_width = pixel_size *. float_of_int camera.image_width in
  let upper_left_corner =
    Pos.create (-.viewport_width /. 2.) (viewport_height /. 2.)
      (-.camera.focal_length)
  in
  let upper_left_pixel =
    Pos.create
      (upper_left_corner.xp +. (pixel_size /. 2.))
      (upper_left_corner.yp -. (pixel_size /. 2.))
      upper_left_corner.zp
  in
  let pixel_delta_u = Vect.scale pixel_size camera.ux in
  let pixel_delta_v = Vect.scale (-.pixel_size) camera.uy in
  let m = Vect.change_of_basis camera.ux camera.uy camera.uz in

  {
    upper_left = upper_left_pixel |> Pos.change_basis m;
    pixel_delta_u = pixel_delta_u |> Vect.change_basis m;
    pixel_delta_v = pixel_delta_v |> Vect.change_basis m;
    viewport_width = camera.image_width;
    viewport_height = camera.image_height;
    pixel_size;
  }

(* let _create_subviewport ~x ~y ~pixel_size camera =
   let viewport_height = pixel_size *. float_of_int camera.image_height in
   let viewport_width = pixel_size *. float_of_int camera.image_width in
   let upper_left_corner = Pos.create x y (-.camera.focal_length) in
   let upper_left_pixel =
     Pos.create
       (upper_left_corner.xp +. (pixel_size /. 2.))
       (upper_left_corner.yp -. (pixel_size /. 2.))
       upper_left_corner.zp
   in
   let pixel_delta_u = Vect.scale pixel_size camera.ux in
   let pixel_delta_v = Vect.scale (-.pixel_size) camera.uy in
   let m = Vect.change_of_basis camera.ux camera.uy camera.uz in
   {
     upper_left = upper_left_pixel |> Pos.change_basis m;
     pixel_delta_u = pixel_delta_u |> Vect.change_basis m;
     pixel_delta_v = pixel_delta_v |> Vect.change_basis m;
     viewport_width;
     viewport_height;
     pixel_size;
   } *)

let create_subviewport ~upper_left ~viewport_width ~viewport_height viewport =
  let upper_left =
    viewport.upper_left
    |> Pos.offset
         (Vect.scale (fst upper_left |> float_of_int) viewport.pixel_delta_u)
    |> Pos.offset
         (Vect.scale (snd upper_left |> float_of_int) viewport.pixel_delta_v)
  in
  { viewport with upper_left; viewport_width; viewport_height }
