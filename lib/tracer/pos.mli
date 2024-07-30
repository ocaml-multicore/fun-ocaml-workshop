(** This module defines position as well as utilitary functions for it. *)

type t = { xp : float; yp : float; zp : float }
(** A position is a defined by its 3 coordinates. *)

val create : float -> float -> float -> t

val vector : t -> t -> Vect.t
(** [vector a b] returns the vector from a to b. *)

val offset : Vect.t -> t -> t
(** [offset v t] translates the position [t] by the vector [v].*)

val dist : t -> t -> float
(** [dist p1 p2] computes the Euclidean distance between two points [p1] and 
[p2]. *)

val change_basis : float array array -> t -> t
(** [change_basis matrix p] transforms the point [p] from the standard basis 
to a new basis defined by the given [matrix]. *)
