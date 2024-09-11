(** This module defines the available forms to describe the scene on which rays 
are casted. *)

(** The current available form are :
{ul 
{- Planes defined by one point and a normal vector}
{- Spheres defined by their center position and their radius.}}

Note : Planes are infinite so they are not very useful for now :). 
*)
type form =
  | Sphere of { centre : Pos.t; radius : float }
  | Plane of { normal : Vect.t; point : Pos.t }
[@@deriving yojson]

type obj = { form : form; material : Material.t } [@@deriving yojson]
(** An object is described by its form and its material. *)

type scene = obj list [@@deriving yojson]

(** {1 Constructors}*)

val sphere : Pos.t -> float -> form
(** [sphere center_pos radius]*)

val plane : Vect.t -> Pos.t -> form
(** [plane normal point] *)

val to_string : scene -> string
(** [to_string scene] *)

val of_string : string -> scene
(** [of_string yojson_scene] *)
