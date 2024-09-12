open Ray_tracer

let f () = Random.float 2.0 -. 1.0

let example5 () =
  let x, y = (f (), f ()) in
  let r, g, b = (Random.float 1.0, Random.float 1.0, Random.float 1.0) in
  let open Scene in
  [
    {
      form = sphere (Pos.create 0. (-1000.5) (-1.)) 1000.;
      material = Material.create_lambertian (Color.rgb 0.5 0.5 0.5);
    };
    {
      form = Scene.sphere (Pos.create x y (-1.)) 0.5;
      material = Material.create_lambertian (Color.rgb r g b);
    };
  ]
  |> objs_to_scene

let example6 () =
  let x = Random.float 1.0 in
  let open Scene in
  [
    {
      form = Scene.sphere (Pos.create x (-0.2) (-0.8)) 0.1;
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
  |> objs_to_scene

let _example_transfo () =
  let open Scene in
  let objs =
    [
      {
        form = sphere (Pos.create 0. 0. (-1.)) 0.5;
        material = Material.create_lambertian (Color.rgb 0.5 0.5 0.5);
      };
      {
        form = sphere (Pos.create 0. 0.5 (-1.)) 0.3;
        material = Material.create_lambertian (Color.rgb 0.5 0. 0.);
      };
    ]
    |> List.map to_obj_t
  in
  let scale1 = build_scale 0.5 in
  let transla1 = build_translation (Vect.create 0.5 0. 0.) in
  let scale2 = build_scale 0.25 in
  let transla2 = build_translation (Vect.create (-0.5) 0. 0.) in
  let rec scene =
    [
      with_transfo ~transfo:[ scale1; transla1 ] (of_list objs);
      Transfo
        {
          transfo = scale2;
          obj = Transfo { transfo = transla2; obj = Compo scene };
        };
    ]
  in
  scene

let scenes = [| example5; example6 |]
let random_scene () = scenes.(Random.int (Array.length scenes)) ()

(* let random_scene = example_transfo *)
