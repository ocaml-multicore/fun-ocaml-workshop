(** This module defines the available forms to describe the scene on which rays 
are casted. *)

(** The current available form are :
{ul 
{- Planes defined by one point and a normal vector}
{- Spheres defined by their center position and their radius.}}

Note : Planes are infinite so they are not very useful for now :). 
*)

(** {1 Forms}*)

type form =
  | Sphere of { centre : Pos.t; radius : float }
  | Plane of { normal : Vect.t; point : Pos.t }
[@@deriving yojson]

val sphere : Pos.t -> float -> form
(** [sphere center_pos radius]*)

val plane : Vect.t -> Pos.t -> form
(** [plane normal point] *)

(** {2  Transformations}*)
type transfo [@@deriving yojson]
(** A transformation is a translation, a scaling or a rotation. *)

val build_translation : Vect.t -> transfo
(** [build_translation vect] *)

val build_scale : float -> transfo
(** [build_scale k] *)

val build_rotation : Pos.t -> float array array -> transfo
(** [build_rotation center matrix] *)

val build_rotation_alt : Pos.t -> Vect.t -> float -> transfo
(** [build_rotation_alt center vect angle] *)

(** {3 Objects} *)

type obj = { form : form; material : Material.t } [@@deriving yojson]
(** An object is described by its form and its material. *)

type obj_t =
  | Objet of obj
  | Transfo of { transfo : transfo; obj : obj_t }
  | Compo of obj_t list
[@@deriving yojson]

val to_obj : ?depth:int -> obj_t -> obj list
(** [to_obj obj_t] *)

val to_obj_t : obj -> obj_t
(** [to_obj_t objs] *)

val with_transfo : ?transfo:transfo list -> obj_t -> obj_t
(** [with_transfo ~transfo obj] *)

val of_list : obj_t list -> obj_t
(** [of_list objs] *)

(** {4 Scene}*)

type scene = obj_t list [@@deriving yojson]

val objs_to_scene : obj list -> scene
(** [objs_to_scene objs] *)

val to_string : scene -> string
(** [to_string scene] *)

val of_string : string -> scene
(** [of_string yojson_scene] *)

(** {5 Advanced functions}*)

val transform : transfo -> obj -> obj
(** [transform transfo form] *)
