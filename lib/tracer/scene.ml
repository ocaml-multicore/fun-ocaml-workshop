type form =
  | Sphere of { centre : Pos.t; radius : float }
  | Plane of { normal : Vect.t; point : Pos.t }
[@@deriving yojson]

type obj = { form : form; material : Material.t } [@@deriving yojson]
type scene = obj list [@@deriving yojson]

let sphere centre radius = Sphere { centre; radius = Float.abs radius }
let plane normal point = Plane { normal = Vect.unit_vector normal; point }
let to_string scene = scene_to_yojson scene |> Yojson.Safe.to_string

let of_string yojson_scene : scene =
  scene_of_yojson @@ Yojson.Safe.from_string yojson_scene |> Result.get_ok

let random_color_related_to_pos ?(min = 0.) x_range y_range x y =
  let scale minv maxv v =
    assert (minv < maxv);
    ((v -. minv) /. (maxv -. minv) *. (1. -. min)) +. min
  in
  Color.rgb
    (scale (fst x_range) (snd x_range) x)
    (scale (fst y_range) (snd y_range) y)
    min
(* (scale (fst x_range +. fst y_range) (snd x_range +. snd y_range) (x +. y)) *)

let random_material ?(pdielec = 0.1) ?(pmetal = 0.35) x_range y_range x y =
  let m = Random.float 1.0 in
  if m < pdielec then Material.create_dielectric Color.white 1.5
  else if m < pdielec +. pmetal then
    Material.create_metal
      (random_color_related_to_pos ~min:0.8 x_range y_range x y)
      (Random.float 0.2)
  else
    Material.create_lambertian
    @@ random_color_related_to_pos x_range y_range x y

let are_colliding obj1 obj2 =
  match (obj1.form, obj2.form) with
  | Sphere sph1, Sphere sph2 ->
      Pos.dist sph1.centre sph2.centre <= sph1.radius +. sph2.radius
  | _, _ -> false

let random_scene ?(x_range = (-11., 11.)) ?(y_range = (-11., 11.))
    ?(max_nobj = 100) () =
  (* Random radius between 0.15 and 0.25 *)
  let max_radius = 0.5 in
  let delta_radius = 0.4 in

  let random_sqrt f = Random.float (sqrt f) *. Random.float (sqrt f) in
  let random_radius () =
    random_sqrt delta_radius +. max_radius -. delta_radius
  in
  let generate (minx, maxx) = Random.float (maxx -. minx) +. minx in
  let rec loop acc count =
    match count with
    | 0 -> acc
    | _ ->
        let aa = generate x_range in
        let bb = generate y_range in
        let radius = random_radius () in
        let centre = Pos.create aa radius bb in
        let sphere_material =
          random_material ~pmetal:0.2 ~pdielec:0.05 x_range y_range aa bb
        in
        let new_obj =
          { form = sphere centre radius; material = sphere_material }
        in
        if List.for_all (fun obj -> not @@ are_colliding new_obj obj) acc then
          loop (new_obj :: acc) (count - 1)
        else loop acc count
  in
  let ground =
    {
      form = sphere (Pos.create 0. (-1000.) 0.) 1000.;
      material = Material.create_lambertian (Color.rgb 0.5 0.5 0.5);
    }
  in
  let sphere1 =
    {
      form = sphere (Pos.create 0. 1. 0.) 1.0;
      material = Material.create_dielectric Color.white 1.5;
    }
  in
  (* let sphere2 =
       {
         form = sphere (Pos.create (-4.) 1. 0.) 1.0;
         material = Material.create_lambertian (Color.rgb 0.13 0.94 0.65);
       }
     in
     let sphere3 =
       {
         form = sphere (Pos.create 4. 1. 0.) 1.0;
         material = Material.create_metal (Color.rgb 0.7 0.6 0.5) 0.0;
       }
     in*)
  let scene = loop [ sphere1 ] (max 0 (max_nobj - 3)) in
  ground :: scene
