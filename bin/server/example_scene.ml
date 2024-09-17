open Ray_tracer

let nb_objects = 4
let half = nb_objects / 2

let build_scene () =
  let open Scene in
  let rec loop acc a b =
    match (a, b) with
    | 0, 0 -> acc
    | 0, _ -> loop acc nb_objects (b - 1)
    | _, _ ->
        let aa = a - half |> float_of_int in
        let bb = b - half |> float_of_int in
        let center =
          Pos.create
            (aa +. (0.9 *. Random.float 1.))
            (0.2 +. Random.float 2.0)
            (bb +. (0.9 *. Random.float 1.))
        in
        let radius = Random.float 0.5 in
        if Pos.vector center (Pos.create 4. 0.2 0.) |> Vect.norm > 1.0 *. radius
        then
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
            { form = Scene.sphere center radius; material = sphere_material }
          in
          loop (sphere :: acc) (a - 1) b
        else loop acc (a - 1) b
  in
  let ground =
    {
      form = Scene.sphere (Pos.create 0. (-1000.) 0.) 1000.;
      material = Material.create_lambertian (Color.random ~min:0.5 ~max:0.8 ());
    }
  in
  let f () = Random.float 2. -. 1. in
  let sphere1 =
    {
      form = Scene.sphere (Pos.create (f ()) (1.0 +. f ()) 0.) 1.0;
      material =
        Material.create_dielectric (Color.random ~min:0.3 ~max:0.8 ()) 1.5;
    }
  in
  let sphere2 =
    {
      form = Scene.sphere (Pos.create (-4. +. f ()) (1.0 +. f ()) 0.) 1.0;
      material = Material.create_lambertian (Color.random ~min:0.3 ~max:0.5 ());
    }
  in
  let sphere3 =
    {
      form = Scene.sphere (Pos.create (4. +. f ()) (1.0 +. f ()) 0.) 1.0;
      material = Material.create_metal (Color.random ~min:0.3 ~max:1.0 ()) 0.0;
    }
  in
  let scene = loop [] nb_objects nb_objects in
  ground :: sphere1 :: sphere2 :: sphere3 :: scene

let final_scene ~image_width ~ratio ~nsamples ~max_depth () =
  let scene = build_scene () in
  let camera =
    Camera.create ~defocus_angle:0.1 ~focus_dist:10. ~vfov:20. ~image_width
      ~ratio ~vup:(Vect.create 0. 1. 0.) ~lookat:(Pos.create 0. 0. 0.)
      ~lookfrom:(Pos.create 13. 2. 3.) ~nsamples ~max_depth ()
  in
  let viewport = Camera.create_viewport camera in
  { Protocol.scene; camera; viewport }
