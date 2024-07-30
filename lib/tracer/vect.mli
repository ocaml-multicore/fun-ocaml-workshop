(** This module defines a type to describe vectors and provides functions to 
manipulate them. *)

type t = { xv : float; yv : float; zv : float }
(** Defines a vector. *)

val create : float -> float -> float -> t
(** [create x y z] creates a new vector with the given x, y, and z components. *)

val add : t -> t -> t
(** [add v1 v2] adds two vectors together. *)

val subtract : t -> t -> t
(** [subtract v1 v2] subtracts the second vector [v2] from the first vector [v1].
 *)

val neg : t -> t
(** [neg v] negates [v] by changing the sign of each component. *)

val scale : float -> t -> t
(** [scale k v] scales [v] by [k]. *)

val norm : t -> float
(** [norm v] calculates the Euclidean norm (length) of the given vector. *)

val norm2 : t -> float
(** [norm2 v] calculates the squared Euclidean norm (length) of the given 
vector. *)

val dot : t -> t -> float
(** [dot v1 v2] calculates the scalar (dot) product of two vectors. *)

val cross : t -> t -> t
(** [cross v1 v2] calculates the cross product of two vectors. *)

val unit_vector : t -> t
(** [unit_vector v] returns the unit vector in the same direction of [v]. *)

val change_of_basis : t -> t -> t -> float array array
(** [change_of_basis v1 v2 v3] calculates the change of basis matrix given three
 basis vectors. *)

val mul_mat_vect : float array array -> t -> t
(** [mul_mat_vect m v] multiplies matrix [m] by the vector [v]. *)

val change_basis : float array array -> t -> t
(** [change_basis m v] changes the basis of [v] using the given change of basis
 matrix [m]. *)

val random : ?min:float -> ?max:float -> unit -> t
(** [random ()] generates a random vector. *)

val random_unit : ?min:float -> ?max:float -> unit -> t
(** [random_unit ()] generates a random unit vector. *)
