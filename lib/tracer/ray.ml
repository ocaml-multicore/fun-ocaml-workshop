type ray = { dir : Vect.t; origin : Pos.t; get : float -> Pos.t }
type rays = ray array array

let dir r = r.dir
let origin r = r.origin

let create dir origin =
  let norm = Vect.norm dir in
  if norm = 0. then failwith "Invalid vector";
  let dir = dir |> Vect.scale (1. /. norm) in
  {
    dir;
    origin;
    get =
      (fun f ->
        Pos.create
          (origin.xp +. (f *. dir.xv))
          (origin.yp +. (f *. dir.yv))
          (origin.zp +. (f *. dir.zv)));
  }

let build_rays (camera : Camera.camera) (viewport : Camera.viewport) : rays =
  let camera_center = Camera.camera_center camera in
  let rays : Vect.t array array =
    Array.make_matrix (Camera.image_height camera) (Camera.image_width camera)
    @@ Vect.create 0. 0. 0.
  in
  Utils.map2Dij
    (fun ii jj _ ->
      let pixel_center =
        viewport.upper_left
        |> Pos.offset (Vect.scale (float_of_int ii) viewport.pixel_delta_u)
        |> Pos.offset (Vect.scale (float_of_int jj) viewport.pixel_delta_v)
      in
      let ray_direction = Pos.vector camera_center pixel_center in
      create ray_direction camera_center)
    rays

(* An intersection point is defined by :
   - its position
   - the object that is hitten
   - a boolean [front_face] that define which side the surface of the object is
     hitten
   - the normal vector of the surface hitten, that always point outward from the
     surface of the object
*)
type hit_point = {
  pos : Pos.t;
  obj : Scene.obj;
  front_face : bool;
  normal : Vect.t;
}

let hit_front_face ray normal = Vect.dot ray.dir normal <= 0.

let[@inline] set_normal is_front_face normal =
  if is_front_face then normal else Vect.neg normal

let[@inline] build_hit_point_sphere ray obj t =
  match Scene.(obj.form) with
  | Sphere sphere ->
      let hit_point = ray.get t in
      let outward_normal =
        Pos.vector sphere.centre hit_point |> Vect.unit_vector
      in
      let front_face = hit_front_face ray outward_normal in
      let normal = set_normal front_face outward_normal in
      { pos = hit_point; obj; front_face; normal }
  | _ -> assert false

let intersect ?(tmin = 0.) ?(tmax = Float.max_float) ray obj =
  match Scene.(obj.form) with
  | Plane plane ->
      let is_par = Vect.dot plane.normal ray.dir in
      if is_par = 0. then None
      else
        let vect_rp = Pos.vector ray.origin plane.point in
        let t = Vect.dot plane.normal vect_rp /. is_par in
        if t < tmin || t > tmax then None
        else
          Some
            {
              front_face = is_par < 0.;
              pos = ray.get t;
              obj;
              normal = set_normal (is_par < 0.) plane.normal;
            }
  | Sphere sphere ->
      let cr = Pos.vector sphere.centre ray.origin in
      (* let a = Vect.norm2 ray.dir in *)
      (* a = 1 because ray.dir is a unit vector *)
      let half_b = Vect.dot cr ray.dir in
      let c = Vect.dot cr cr -. (sphere.radius *. sphere.radius) in
      let delta = (half_b *. half_b) -. c in
      if delta < 0. then None
      else
        let t1 = -.half_b -. sqrt delta in
        if Float.abs t1 < tmin || t1 > tmax then None
        else if t1 < 0. then
          let t2 = -.half_b +. sqrt delta in
          if t2 < tmin || t2 > tmax then None
          else Some (build_hit_point_sphere ray obj t2)
        else Some (build_hit_point_sphere ray obj t1)

(* let test () =
   let circle = Scene.sphere (Pos.create 0. 0. 0.) 1. in
   let ray = Ray.create (Vect.create 0. (-1.) 0.) (Pos.create 0. 10. 0.) in
   Ray.intersection ray circle *)

let sky_color ray =
  let unit_dir = Vect.unit_vector (dir ray) in
  let a = 0.5 *. (Vect.(unit_dir.yv) +. 1.0) in
  let r, g, b = (1.0 -. (0.5 *. a), 1.0 -. (0.3 *. a), 1.0) in
  Color.rgb r g b

let compute_hit_point (scene : Scene.scene) ray =
  let rec loop ray closest scene =
    match scene with
    | [] -> closest
    | obj :: tl -> begin
        match intersect ~tmin:0.0001 ray obj with
        | None -> loop ray closest tl
        | Some hit_point as curr -> (
            match closest with
            | None -> loop ray curr tl
            | Some prev_hit_point ->
                if
                  Pos.dist ray.origin hit_point.pos
                  < Pos.dist ray.origin prev_hit_point.pos
                then loop ray curr tl
                else loop ray closest tl)
      end
  in
  loop ray None scene

let ray_to_color ~max_depth scene ray =
  let rec loop ray depth =
    if depth <= 0 then Color.black
    else
      match compute_hit_point scene ray with
      | None -> sky_color ray
      | Some hit_point -> begin
          match hit_point.obj.material with
          | Flat color -> color
          | Lambertian albedo ->
              let reflect_dir =
                Material.lambertian_reflection hit_point.normal
              in
              let reflected_ray = create reflect_dir hit_point.pos in
              let reflected_color = loop reflected_ray (depth - 1) in
              Color.mul albedo reflected_color
          | Metal { albedo; fuzz } -> (
              match Material.metal_reflection ray.dir hit_point.normal fuzz with
              | None -> Color.black
              | Some reflect_dir ->
                  let reflected_ray = create reflect_dir hit_point.pos in
                  let reflected_color = loop reflected_ray (depth - 1) in
                  Color.mul albedo reflected_color)
          | Dielectric { refraction_index; albedo } ->
              let ri =
                if hit_point.front_face then 1. /. refraction_index
                else refraction_index
              in
              let refracted_dir =
                Material.refract ray.dir hit_point.normal ri
              in
              let refracted_ray = create refracted_dir hit_point.pos in
              let refracted_color = loop refracted_ray (depth - 1) in
              Color.mul albedo refracted_color
        end
  in
  loop ray max_depth

let randomly_move_ray ray deltau deltav =
  let du = Vect.scale (Random.float 1. -. 0.5) deltau in
  let dv = Vect.scale (Random.float 1. -. 0.5) deltav in
  Vect.add (Vect.add ray du) dv |> Vect.unit_vector

let sample_pixel ~nsamples ~max_depth viewport scene ray =
  let rec loop acc_color n =
    match n with
    | 0 -> Color.avg nsamples acc_color |> Color.clamp
    | _ ->
        let new_ray =
          create
            (randomly_move_ray ray.dir
               Camera.(viewport.pixel_delta_u)
               Camera.(viewport.pixel_delta_v))
            ray.origin
        in

        let color = ray_to_color ~max_depth scene new_ray in
        loop (Color.add acc_color color) (n - 1)
  in
  loop Color.black nsamples

let rays_to_colors ?(progress_bar = false) ?(nsamples = 1) ?(max_depth = 10)
    (scene : Scene.scene) viewport (rays : rays) =
  Utils.map2D ~progress_bar
    (fun ray ->
      let color =
        if nsamples = 1 then ray_to_color ~max_depth scene ray
        else sample_pixel ~max_depth ~nsamples viewport scene ray
      in
      Color.linear_to_gamma color)
    rays
