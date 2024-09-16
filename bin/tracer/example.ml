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
  Ray.rays_to_colors ~progress_bar scene camera viewport |> convert_colors

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
  Ray.rays_to_colors ~progress_bar scene camera viewport |> convert_colors

let example7 ?(progress_bar = false) ~image_width ~ratio () =
  let camera =
    Camera.create ~defocus_angle:2. ~vfov:80. ~image_width ~ratio
      ~vup:(Vect.create 0. 1. 0.) ~lookat:(Pos.create 0. 0. (-1.))
      ~lookfrom:(Pos.create (-2.) 2. 1.) ~nsamples:20 ~max_depth:20 ()
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
  Ray.rays_to_colors ~progress_bar scene camera viewport |> convert_colors

let build_scene () =
  let open Scene in
  let rec loop acc a b =
    match (a, b) with
    | 0, 0 -> acc
    | 0, _ -> loop acc 22 (b - 1)
    | _, _ ->
        let aa = a - 11 |> float_of_int in
        let bb = b - 11 |> float_of_int in
        let center =
          Pos.create
            (aa +. (0.9 *. Random.float 1.))
            0.2
            (bb +. (0.9 *. Random.float 1.))
        in
        if Pos.vector center (Pos.create 4. 0.2 0.) |> Vect.norm > 0.9 then
          let sphere_material =
            let choose_mat = Random.float 1. in
            if choose_mat < 0.8 then
              Material.create_lambertian
                (Color.mul (Color.random ()) (Color.random ()))
            else if choose_mat < 0.95 then
              Material.create_metal
                (Color.random ~min:0.5 ~max:1. ())
                (Random.float 0.5)
            else Material.create_dielectric Color.white 1.5
          in
          let sphere =
            { form = Scene.sphere center 0.2; material = sphere_material }
          in
          loop (sphere :: acc) (a - 1) b
        else loop acc (a - 1) b
  in
  let ground =
    {
      form = Scene.sphere (Pos.create 0. (-1000.) 0.) 1000.;
      material = Material.create_lambertian (Color.rgb 0.5 0.5 0.5);
    }
  in
  let sphere1 =
    {
      form = Scene.sphere (Pos.create 0. 1. 0.) 1.0;
      material = Material.create_dielectric Color.white 1.5;
    }
  in
  let sphere2 =
    {
      form = Scene.sphere (Pos.create (-4.) 1. 0.) 1.0;
      material = Material.create_lambertian (Color.rgb 0.4 0.2 0.1);
    }
  in
  let sphere3 =
    {
      form = Scene.sphere (Pos.create 4. 1. 0.) 1.0;
      material = Material.create_metal (Color.rgb 0.7 0.6 0.5) 0.0;
    }
  in
  let scene = loop [] 22 22 in
  ground :: sphere1 :: sphere2 :: sphere3 :: scene

let final_scene ?(progress_bar = false) ~image_width ~ratio () =
  let scene = build_scene () in
  let camera =
    Camera.create ~defocus_angle:0.1 ~focus_dist:10. ~vfov:20. ~image_width
      ~ratio ~vup:(Vect.create 0. 1. 0.) ~lookat:(Pos.create 0. 0. 0.)
      ~lookfrom:(Pos.create 13. 2. 3.) ~nsamples:300 ~max_depth:50 ()
  in
  let viewport = Camera.create_viewport camera in
  Ray.rays_to_colors ~progress_bar scene camera viewport |> convert_colors

type task = { ulpix : int * int; subviewport : Camera.viewport }

let worker scene camera pool res_queue () =
  let rec loop () =
    match Saturn_lockfree.Queue.pop_opt pool with
    | None -> ()
    | Some { ulpix; subviewport } ->
        (* Format.printf "Worker Got %d %d\n%!" (fst ulpix) (snd ulpix); *)
        let colors =
          Ray.rays_to_colors scene camera subviewport |> convert_colors
        in
        Saturn_lockfree.Single_consumer_queue.push res_queue (ulpix, colors);
        loop ()
  in
  loop ()

let final_scene_par ~image_width ~ratio ~subviewport_length ~subviewport_width
    res_queue () =
  let x_range = (-10., 8.) in
  let y_range = (-4., 4.) in
  let scene = Scene.random_scene ~max_nobj:100 ~x_range ~y_range () in

  let camera =
    Camera.create ~defocus_angle:0.1 ~focus_dist:10. ~vfov:20. ~image_width
      ~ratio ~vup:(Vect.create 0. 1. 0.) ~lookat:(Pos.create 0. 0. 0.)
      ~lookfrom:(Pos.create 13. 2. 3.) ~nsamples:100 ~max_depth:20 ()
  in
  let viewport = Camera.create_viewport camera in

  let divu = image_width / subviewport_width in
  let divv = image_width / subviewport_length in

  let task_queue = Saturn_lockfree.Queue.create () in
  let rec build_queue queue u v =
    if u = divu then ()
    else if v = divv then build_queue queue (u + 1) 0
    else
      let ulpix = (u * subviewport_width, v * subviewport_length) in
      let subviewport =
        Camera.create_subviewport ~upper_left:ulpix
          ~viewport_width:subviewport_width ~viewport_height:subviewport_length
          viewport
      in
      Saturn_lockfree.Queue.push task_queue { ulpix; subviewport };
      build_queue queue u (v + 1)
  in
  build_queue task_queue 0 0;
  Array.init
    (Domain.recommended_domain_count () - 1)
    (fun _ -> Domain.spawn (worker scene camera task_queue res_queue))
