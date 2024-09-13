type camera = {
  camera_center : Pos.t;
  ux : Vect.t;
  uy : Vect.t;
  uz : Vect.t;
  image_width : int;
  image_height : int;
  focal_length : float;
  vfov : float;
}
[@@deriving yojson]

let camera_center camera = camera.camera_center
let image_width camera = camera.image_width
let image_height camera = camera.image_height

let create ?(ratio = 16. /. 9.) ?(image_width = 1600) ?(vfov = 90.) ~vup ~lookat
    ~lookfrom () =
  let image_height =
    int_of_float (float_of_int image_width /. ratio) |> max 1
  in
  let focal_length = Pos.dist lookat lookfrom in
  let uz = Vect.unit_vector (Pos.vector lookat lookfrom) in
  let ux = Vect.unit_vector (Vect.cross vup uz) in
  let uy = Vect.cross uz ux in
  {
    ux;
    uy;
    uz;
    camera_center = lookfrom;
    image_width;
    image_height;
    focal_length;
    vfov;
  }

let default =
  create ~vup:(Vect.create 0. 1. 0.) ~lookfrom:(Pos.create 0. 0. 0.)
    ~lookat:(Pos.create 0. 0. (-1.)) ()

type viewport = {
  upper_left : Pos.t;
  pixel_delta_u : Vect.t;
  pixel_delta_v : Vect.t;
  viewport_width : int;
  viewport_height : int;
  pixel_size : float;
}
[@@deriving yojson]

let create_viewport camera =
  let theta = Utils.degrees_to_radians camera.vfov in
  let viewport_height = 2.0 *. Float.tan (theta /. 2.) *. camera.focal_length in
  let pixel_size = viewport_height /. float_of_int camera.image_height in
  let viewport_width = pixel_size *. float_of_int camera.image_width in
  let viewport_u = Vect.scale viewport_width camera.ux in
  let viewport_v = Vect.scale (-1. *. viewport_height) camera.uy in
  let pixel_delta_u =
    Vect.scale (1. /. float_of_int camera.image_width) viewport_u
  in
  let pixel_delta_v =
    Vect.scale (1. /. float_of_int camera.image_height) viewport_v
  in
  let ul_corner =
    camera.camera_center
    |> Pos.offset (Vect.scale (-.viewport_width /. 2.) camera.ux)
    |> Pos.offset (Vect.scale (viewport_height /. 2.) camera.uy)
    |> Pos.offset (Vect.scale (-.camera.focal_length) camera.uz)
  in
  let ul_pixel =
    Pos.offset (Vect.scale (pixel_size /. 2.) camera.ux) ul_corner
  in
  {
    upper_left = ul_pixel;
    pixel_delta_u;
    pixel_delta_v;
    viewport_width = camera.image_width;
    viewport_height = camera.image_height;
    pixel_size;
  }

let create_subviewport ~upper_left ~viewport_width ~viewport_height viewport =
  let upper_left =
    viewport.upper_left
    |> Pos.offset
         (Vect.scale (fst upper_left |> float_of_int) viewport.pixel_delta_u)
    |> Pos.offset
         (Vect.scale (snd upper_left |> float_of_int) viewport.pixel_delta_v)
  in
  { viewport with upper_left; viewport_width; viewport_height }
