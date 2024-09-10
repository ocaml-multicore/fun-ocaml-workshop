(* Diffuse *)
type t =
  | Flat of Color.t
  | Lambertian of Color.t
  | Metal of { albedo : Color.t; fuzz : float }
  | Dielectric of { albedo : Color.t; refraction_index : float }
[@@deriving yojson]

let create_lambertian albedo = Lambertian albedo
let create_flat color = Flat color
let create_metal albedo fuzz = Metal { albedo; fuzz = max 0. (min 1. fuzz) }

let create_dielectric albedo refraction_index =
  Dielectric { albedo; refraction_index }

let[@inline] lambertian_reflection normal =
  (* normal is supposed to be an unit_vector but because of float error it may
     not be anymore *)
  try
    Vect.add (normal |> Vect.unit_vector) (Vect.random_unit ())
    |> Vect.unit_vector
  with _ -> normal

let[@inline] metal_reflection incidence normal fuzz =
  (* let incidence = Vect.unit_vector incidence in *)
  let b =
    normal |> Vect.scale (Vect.dot incidence normal) |> Vect.scale (-2.)
  in
  let reflected = Vect.add incidence b |> Vect.unit_vector in
  if Vect.dot reflected normal > 0. then
    Some (Vect.add reflected (Vect.scale fuzz (Vect.random_unit ())))
  else None

let[@inline] refract incident normal rindex_ratio =
  let cos_theta = min (Vect.dot (Vect.neg incident) normal) 1. in
  let r_out_perpendicular =
    Vect.scale rindex_ratio @@ Vect.add incident (Vect.scale cos_theta normal)
  in
  let r_out_parallel =
    Vect.scale (-.sqrt (1. -. Vect.norm2 r_out_perpendicular)) normal
  in
  Vect.add r_out_perpendicular r_out_parallel |> Vect.unit_vector
