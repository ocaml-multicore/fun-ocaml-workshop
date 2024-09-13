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
        if t1 < tmin || t1 > tmax then None
        else if t1 < 0. then
          let t2 = -.half_b +. sqrt delta in
          if t2 < tmin || t2 > tmax then None
          else Some (build_hit_point_sphere ray obj t2)
        else Some (build_hit_point_sphere ray obj t1)

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

let ray_to_color_ max_depth scene ray =
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
              let cos_theta =
                min (Vect.dot (Vect.neg ray.dir) hit_point.normal) 1.
              in
              let sin_theta = sqrt (1. -. (cos_theta *. cos_theta)) in
              let cannot_refract = ri *. sin_theta > 1. in

              let new_dir =
                if cannot_refract || Material.schlick_approximation cos_theta ri
                then Material.reflect ray.dir hit_point.normal
                else Material.refract ray.dir hit_point.normal ri
              in
              let new_ray = create new_dir hit_point.pos in
              let new_color = loop new_ray (depth - 1) in
              Color.mul albedo new_color
        end
  in
  loop ray max_depth

let randomly_move_ray ray deltau deltav =
  let du = Vect.scale (Random.float 1. -. 0.5) deltau in
  let dv = Vect.scale (Random.float 1. -. 0.5) deltav in
  Vect.add (Vect.add ray du) dv |> Vect.unit_vector

let ray_to_color ?(nsamples = 1) ?(max_depth = 10) scene viewport ray =
  let rec loop acc_color n =
    match n with
    | 0 -> Color.avg nsamples acc_color
    | _ ->
        let new_ray =
          create
            (randomly_move_ray ray.dir
               Camera.(viewport.pixel_delta_u)
               Camera.(viewport.pixel_delta_v))
            ray.origin
        in

        let color = ray_to_color_ max_depth scene new_ray in
        loop (Color.add acc_color color) (n - 1)
  in
  (if nsamples = 1 then ray_to_color_ max_depth scene ray
   else loop Color.black nsamples)
  |> Color.clamp |> Color.linear_to_gamma

let[@inline] print_progress height jj =
  if jj mod 100 = 0 then
    Format.printf "\rScanlines remaining: %d @." (height - jj)

(* point3 defocus_disk_sample() const {
       // Returns a random point in the camera defocus disk.
       auto p = random_in_unit_disk();
       return center + (p[0] * defocus_disk_u) + (p[1] * defocus_disk_v);
   }
*)
let[@inline] defocus_disk_sample (camera : Camera.camera) =
  let p = Vect.random_in_unit_disk () in
  let x = Vect.scale p.xv camera.defocus_disk_u in
  let y = Vect.scale p.yv camera.defocus_disk_v in
  Pos.offset (Vect.add x y) camera.center

let rays_to_colors ?(progress_bar = false) (scene : Scene.scene)
    (camera : Camera.camera) (viewport : Camera.viewport) =
  let max_depth = camera.max_depth in
  let nsamples = camera.nsamples in
  let viewport_height = viewport.viewport_height in
  Array.init_matrix viewport.viewport_height viewport.viewport_width
    (fun jj ii ->
      if progress_bar && ii = 0 then print_progress viewport_height jj;

      let pixel_center =
        viewport.upper_left
        |> Pos.offset (Vect.scale (float_of_int ii) viewport.pixel_delta_u)
        |> Pos.offset (Vect.scale (float_of_int jj) viewport.pixel_delta_v)
      in
      let ray_origin =
        if camera.defocus_angle <= 0. then camera.center
        else defocus_disk_sample camera
      in

      let ray_direction = Pos.vector ray_origin pixel_center in
      let ray = create ray_direction ray_origin in
      ray_to_color ~max_depth ~nsamples scene viewport ray)
