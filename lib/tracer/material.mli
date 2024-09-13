(** This module defines material and the way they refract and/or reflect light. 
*)

(** Represents the different type of implemented materials. Use the following
 builder functions to ensure the parameters are in the proper range.
 The available materials are : 
{ul 
{- [Flat color] : a ray that hits this material is colored with [color].}
{- [Lambertian albedo] : a diffusing material. A ray that hits it is reflected 
according to lambertian reflection.[albedo] defines the absorption of the 
material.}
{- [Metal {albedo; fuzz}] : a metal like material. A ray that hits this material 
is reflected the same way a mirror do. [fuzz] should be between [0] and [1]. It
 adds some randomness to the reflected ray direction to simulate unpolishness of 
 the material ([0] = perfect mirror). }
{- [Dielectric {albedo; refraction_index}] : WORK in PROGRESS. A transparent 
material.}}

Most of the material are defined by a albedo (a {!Color.t}). It defines how the 
material reflects (and absorbs) incoming colors. For example, an [albedo] of 
[(1, 0, 0)] is letting all red light pass (via reflection or refraction) and non
 of the other colors. 

 For non-flat materials, the number of time a ray can be bounced through 
 reflection or refraction is defined by the parameter [max_depth] of the 
 function {!Ray.rays_to_colors}.
 *)
type t =
  | Flat of Color.t
  | Lambertian of Color.t
  | Metal of { albedo : Color.t; fuzz : float }
  | Dielectric of { albedo : Color.t; refraction_index : float }
[@@deriving yojson]

(** {1 Constructors} *)

val create_flat : Color.t -> t
(** [create_flat color] *)

val create_lambertian : Color.t -> t
(** [create_lambertian albedo] *)

val create_metal : Color.t -> float -> t
(** [create_metal albedo fuzz] *)

val create_dielectric : Color.t -> float -> t
(** [create_dielectric albedo refraction_index] *)

(** {1 Reflection and refraction functions} *)

val lambertian_reflection : Vect.t -> Vect.t
(** [lambertian_reflection ray] *)

val metal_reflection : Vect.t -> Vect.t -> float -> Vect.t option
(** [metal_reflection ray normal fuzz] *)

val refract : Vect.t -> Vect.t -> float -> Vect.t
(** [refract ray normal refraction_index] *)

val reflect : Vect.t -> Vect.t -> Vect.t
(** [reflect ray normal] *)

val schlick_approximation : float -> float -> bool
(** [schlick_approximation cos refraction_index] *)
