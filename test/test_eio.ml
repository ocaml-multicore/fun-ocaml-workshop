module Test_scene = struct
  open Ray_tracer

  let standart_config ~image_width ~ratio ~x ~y ~pixel_size =
    let camera =
      Camera.create ~image_width ~ratio ~camera_center:(Pos.create 0. 0. 0.)
        ~focal_length:1. ()
    in
    let viewport = Camera.create_subviewport ~x ~y ~pixel_size camera in
    (camera, viewport)

  let _example5 ?(progress_bar = false) ~x ~y ~w ~h ~pixel_size () =
    let ratio = float w /. float h in
    let camera, viewport =
      standart_config ~image_width:w ~ratio ~x ~y ~pixel_size
    in
    let scene : Scene.scene =
      let open Scene in
      [
        {
          form = sphere (Pos.create 0. (-1000.5) (-1.)) 1000.;
          material = Material.create_lambertian (Color.rgb 0.5 0.5 0.5);
        };
        {
          form = Scene.sphere (Pos.create 0. 0. (-1.)) 0.5;
          material = Material.create_lambertian (Color.rgb 0.4 0.2 0.);
        };
      ]
    in
    Ray.rays_to_colors ~progress_bar ~nsamples:50 ~max_depth:20 scene camera
      viewport

  let example6 ?(progress_bar = false) ~x ~y ~w ~h ~pixel_size () =
    let ratio = float w /. float h in
    let camera, viewport =
      standart_config ~image_width:w ~ratio ~x ~y ~pixel_size
    in
    let scene : Scene.scene =
      let open Scene in
      [
        {
          form = Scene.sphere (Pos.create 0.4 (-0.2) (-0.8)) 0.1;
          material = Material.create_metal (Color.rgb 0.8 0.0 0.2) 0.2;
        };
        {
          form = Scene.sphere (Pos.create (-1.) 0. (-1.)) 0.5;
          material = Material.create_metal (Color.rgb 0.8 0.6 0.2) 0.8;
        };
        {
          form = sphere (Pos.create 0. (-100.5) (-1.)) 100.;
          material = Material.create_lambertian (Color.rgb 0.8 0.8 0.);
        };
        {
          form = Scene.sphere (Pos.create 0. (-0.3) (-1.2)) 0.5;
          material = Material.create_lambertian (Color.rgb 0.1 0.2 0.5);
        };
        {
          form = Scene.sphere (Pos.create 1. 0. (-1.)) 0.5;
          material = Material.create_metal (Color.rgb 0.8 0.9 0.8) 0.1;
        };
      ]
    in
    Ray.rays_to_colors ~progress_bar ~nsamples:50 ~max_depth:20 scene camera
      viewport

  let render ~x ~y ~w ~h ~pixel_size =
    example6 ~progress_bar:true ~x ~y ~w ~h ~pixel_size ()
end

module Actor = Actor_eio

let s = 1408.0
let zoom = 2.0
let pixel_size = zoom /. s

let myimage x y w h =
  let sc v = (1.0 *. float v /. s) -. 0.5 in
  let arr =
    Test_scene.render
      ~x:(zoom *. sc x)
      ~y:(zoom *. (0.0 -. sc y))
      ~w ~h ~pixel_size
  in
  let img = Image.create_rgb w h in
  for x = 0 to w - 1 do
    for y = 0 to h - 1 do
      let { Ray_tracer.Color.r; g; b } = arr.(y).(x) in
      let f c = int_of_float (c *. 255.0) in
      let r, g, b = (f r, f g, f b) in
      Image.write_rgb img x y r g b
    done
  done;
  img

let rec split (sub : Protocol.sub) n =
  if n = 0 then [ sub ]
  else
    let left, right =
      if sub.w > sub.h then Actor.vsplit sub else Actor.hsplit sub
    in
    let n = n - 1 in
    split left n @ split right n

let username = Sys.argv.(1)

let () =
  Eio_main.run @@ fun env ->
  let _clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  let dmgr = Eio.Stdenv.domain_mgr env in

  let client = Actor.client ~net ~username in

  Eio.Fiber.all
  @@ List.init (Domain.recommended_domain_count ())
  @@ fun _ () ->
  Eio.Domain_manager.run dmgr @@ fun () ->
  while true do
    let sub = Actor.request client in
    let parts = split sub 3 in
    List.iter
      (fun sub ->
        let open Protocol in
        let img = myimage sub.x sub.y sub.w sub.h in
        Actor.respond client sub img)
      parts
  done
