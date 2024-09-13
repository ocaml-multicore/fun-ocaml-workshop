open Ray_tracer

let convert_color Color.{ r; g; b } =
  let foo a = 255. *. a |> Int.of_float in
  Graphics.rgb (foo r) (foo g) (foo b)

let convert_colors image =
  Array.map (fun row -> Array.map convert_color row) image

let standart_config () =
  let camera = Camera.default in
  let viewport = Camera.create_viewport camera in
  (camera, viewport)

let example5 ?(progress_bar = false) () =
  let camera, viewport = standart_config () in
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
  Ray.rays_to_colors ~progress_bar ~nsamples:20 ~max_depth:20 scene
    (Camera.camera_center camera)
    viewport
  |> convert_colors

let example6 ?(progress_bar = false) () =
  let camera, viewport = standart_config () in
  let scene : Scene.scene =
    let open Scene in
    [
      {
        form = sphere (Pos.create 0. (-100.5) (-1.)) 100.;
        material = Material.create_lambertian (Color.rgb 0.8 0.8 0.);
      };
      {
        form = Scene.sphere (Pos.create 0. 0. (-1.2)) 0.5;
        material = Material.create_lambertian (Color.rgb 0.1 0.2 0.5);
      };
      {
        (*  Air bubble *)
        form = Scene.sphere (Pos.create (-1.) 0. (-1.)) 0.4;
        material = Material.create_dielectric (Color.rgb 1. 1. 1.) (1. /. 1.5);
      };
      {
        (* Glass *)
        form = Scene.sphere (Pos.create (-1.) 0. (-1.)) 0.5;
        material = Material.create_dielectric (Color.rgb 1. 1. 1.) 1.5;
      };
      {
        form = Scene.sphere (Pos.create 1. 0. (-1.)) 0.5;
        material = Material.create_metal (Color.rgb 0.8 0.6 0.2) 1.0;
      };
    ]
  in
  Ray.rays_to_colors ~progress_bar ~nsamples:20 ~max_depth:10 scene
    (Camera.camera_center camera)
    viewport
  |> convert_colors

let example7 ?(progress_bar = false) ~image_width ~ratio () =
  let camera =
    Camera.create ~vfov:90. ~image_width ~ratio ~vup:(Vect.create 0. 1. 0.)
      ~lookat:(Pos.create 0. 0. (-1.)) ~lookfrom:(Pos.create (-2.) 2. 1.) ()
  in
  let viewport = Camera.create_viewport camera in
  let scene : Scene.scene =
    let open Scene in
    [
      {
        form = sphere (Pos.create 0. (-100.5) (-1.)) 100.;
        material = Material.create_lambertian (Color.rgb 0.8 0.8 0.);
      };
      {
        form = Scene.sphere (Pos.create 0. 0. (-1.2)) 0.5;
        material = Material.create_lambertian (Color.rgb 0.1 0.2 0.5);
      };
      {
        (*  Air bubble *)
        form = Scene.sphere (Pos.create (-1.) 0. (-1.)) 0.4;
        material = Material.create_dielectric (Color.rgb 1. 1. 1.) (1. /. 1.5);
      };
      {
        (* Glass *)
        form = Scene.sphere (Pos.create (-1.) 0. (-1.)) 0.5;
        material = Material.create_dielectric (Color.rgb 1. 1. 1.) 1.5;
      };
      {
        form = Scene.sphere (Pos.create 1. 0. (-1.)) 0.5;
        material = Material.create_metal (Color.rgb 0.8 0.6 0.2) 1.0;
      };
    ]
  in
  Ray.rays_to_colors ~progress_bar ~nsamples:20 ~max_depth:20 scene
    (Camera.camera_center camera)
    viewport
  |> convert_colors
