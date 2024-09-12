type form =
  | Sphere of { centre : Pos.t; radius : float }
  | Plane of { normal : Vect.t; point : Pos.t }
[@@deriving yojson]

type transfo =
  | Translate of Vect.t
  | Scale of float
  | Rotation of { center : Pos.t; matrix : float array array }
[@@deriving yojson]

type obj = { form : form; material : Material.t } [@@deriving yojson]

type obj_t =
  | Objet of obj
  | Transfo of { transfo : transfo; obj : obj_t }
  | Compo of obj_t list
[@@deriving yojson]

type scene = obj_t list [@@deriving yojson]

let objs_to_scene objs = List.map (fun obj -> Objet obj) objs
let sphere centre radius = Sphere { centre; radius = Float.abs radius }
let plane normal point = Plane { normal = Vect.unit_vector normal; point }
let to_string scene = scene_to_yojson scene |> Yojson.Safe.to_string

let of_string yojson_scene : scene =
  scene_of_yojson @@ Yojson.Safe.from_string yojson_scene |> Result.get_ok

let build_translation vect = Translate vect
let build_scale k = Scale k
let build_rotation center matrix = Rotation { center; matrix }

let compute_rotation_matrix vect angle =
  let open Vect in
  let cos_a = Float.cos angle in
  let sin_a = Float.sin angle in
  let cos_a_minus = 1. -. cos_a in
  let mat = Array.make_matrix 3 3 0. in
  mat.(0).(0) <- cos_a +. (cos_a_minus *. vect.xv *. vect.xv);
  mat.(0).(1) <- (cos_a_minus *. vect.xv *. vect.yv) -. (sin_a *. vect.zv);
  mat.(0).(2) <- (cos_a_minus *. vect.xv *. vect.zv) +. (sin_a *. vect.yv);
  mat.(1).(0) <- (cos_a_minus *. vect.yv *. vect.xv) +. (sin_a *. vect.zv);
  mat.(1).(1) <- cos_a +. (cos_a_minus *. vect.yv *. vect.yv);
  mat.(1).(2) <- (cos_a_minus *. vect.yv *. vect.zv) -. (sin_a *. vect.xv);
  mat.(2).(0) <- (cos_a_minus *. vect.zv *. vect.xv) -. (sin_a *. vect.yv);
  mat.(2).(1) <- (cos_a_minus *. vect.zv *. vect.yv) +. (sin_a *. vect.xv);
  mat.(2).(2) <- cos_a +. (cos_a_minus *. vect.zv *. vect.zv);
  mat

(* let test () =
   let rvect = Vect.create 0. 0. (-1.) in
   let rangle = Float.pi /. 2. in
   let vect = Vect.create 1. 0. 0. in
   let rot_mat = compute_rotation_matrix rvect rangle in
   let r = Vect.change_basis rot_mat vect in
   assert (
     Float.abs (r.xv -. 0.) < Float.epsilon
     && Float.abs (r.yv -. 1.) < Float.epsilon
     && Float.abs (r.zv -. 0.) < Float.epsilon) *)

let build_rotation_alt center vect angle =
  let mat = compute_rotation_matrix vect angle in
  Rotation { center; matrix = mat }

let translate vect form =
  match form with
  | Sphere sphere ->
      Sphere { sphere with centre = Pos.offset vect sphere.centre }
  | Plane plane -> Plane { plane with point = Pos.offset vect plane.point }

let scale k form =
  let origin = Pos.create 0. 0. 0. in
  match form with
  | Sphere sphere ->
      let centre =
        Pos.offset (Pos.vector sphere.centre origin |> Vect.scale k) origin
      in
      Sphere { radius = k *. sphere.radius; centre }
  | Plane _ -> form

let rotation rot_center rot_matrix form =
  match form with
  | Sphere sphere ->
      let rotc_c = Pos.vector rot_center sphere.centre in
      let vect = Vect.change_basis rot_matrix rotc_c in
      let new_center = Pos.offset vect rot_center in
      Sphere { sphere with centre = new_center }
  | Plane plane ->
      let normal = Vect.change_basis rot_matrix plane.normal in
      let rotc_p = Pos.vector rot_center plane.point in
      let vect = Vect.change_basis rot_matrix rotc_p in
      let point = Pos.offset vect rot_center in
      Plane { normal; point }

(* let test () =
   let rot_center = Pos.create 0. 0. 0. in
   let rot_matrix =
     compute_rotation_matrix (Vect.create 0. 0. 1.) (Float.pi /. 2.)
   in
   let form = Sphere { centre = Pos.create 1. 0. 0.; radius = 1. } in
   let form = rotation rot_center rot_matrix form in
   assert (
     match form with
     | Sphere sph ->
         Vect.norm (Pos.vector sph.centre (Pos.create 0. 1. 0.)) < Float.epsilon
         && Float.abs (sph.radius -. 1.) < Float.epsilon
     | _ -> false)
*)
let transform transfo obj =
  let transform_obj =
    match transfo with
    | Translate vect -> translate vect obj.form
    | Scale k -> scale k obj.form
    | Rotation { center; matrix } -> rotation center matrix obj.form
  in
  { obj with form = transform_obj }

let to_obj ?(depth = 3) obj_t =
  let rec loop d = function
    | Objet obj -> obj :: []
    | Transfo { transfo; obj } -> List.map (transform transfo) (loop d obj)
    | Compo objs ->
        if d = 0 then [] else List.map (loop (d - 1)) objs |> List.flatten
  in
  loop depth obj_t

let to_obj_t obj = Objet obj

let rec with_transfo ?(transfo = []) obj_t =
  match transfo with
  | [] -> obj_t
  | transfo :: rest ->
      Transfo { transfo; obj = with_transfo ~transfo:rest obj_t }

let of_list objs = Compo objs
