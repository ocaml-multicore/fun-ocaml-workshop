(** This module provides functionality for working with RGB colors. *)

type t = { r : float; g : float; b : float } [@@deriving yojson]
(** An RGB color represented by its red, green, and blue values. *)

val rgb : float -> float -> float -> t
(** [rgb r g b] creates a color with the given red, green, and blue values. *)

val white : t
(** The predefined white color. *)

val black : t
(** The predefined black color. *)

val add : t -> t -> t
(** [add c1 c2] returns the sum of two colors [c1] and [c2]. *)

val avg : int -> t -> t
(** [avg n c] divides each color values of [c] by the factor [n]. [n] must be 
strictly greater than 0. Used to compute average. *)

val scale : float -> t -> t
(** [scale f c] scales the color [c] by the factor [f]. *)

val clamp : t -> t
(** [clamp c] clamps the color [c] to the range [0.0, 1.0]. *)

val linear_to_gamma : t -> t
(** [linear_to_gamma c] converts the linear color [c] to gamma-corrected color. *)

val mul : t -> t -> t
(** [mul c1 c2] returns the element-wise multiplication of two colors [c1] and 
[c2]. *)

val random : ?min:float -> ?max:float -> unit -> t
(** [random ()] returns a random color. *)
