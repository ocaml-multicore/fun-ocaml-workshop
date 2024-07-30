val distance : float -> float -> float -> float
(** [distance x y z] calculates [sqrt (x*.x +. y*.y +. z*.z)]. *)

val distance2 : float -> float -> float -> float
(** [distance2 x y z] calculates [x*.x +. y*.y +. z*.z]. *)

val degrees_to_radians : float -> float
(** [degrees_to_radians degrees] converts the given angle in degrees to radians. 
*)

val map2Dij :
  ?progress_bar:bool ->
  (int -> int -> 'a -> 'b) ->
  'a array array ->
  'b array array
(** [map2Dij ?progress_bar f arr] is a map with indexes on [arr]. *)

val map2D : ?progress_bar:bool -> ('a -> 'b) -> 'a array array -> 'b array array
(** [map2D ?progress_bar f arr] is a map without indexes on [arr]. *)

val close_enough_float : ?epsilon:float -> float -> float -> bool
(** [close_enough_float ?epsilon x y] checks if the absolute difference between 
    [x] and [y] is less than or equal to the optional epsilon value. If epsilon 
    is not provided, [Float.epsilon] is used. *)
