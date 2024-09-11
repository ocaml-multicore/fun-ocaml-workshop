(** This module defines the camera part of the ray tracer configuration. This 
is where the size of the image is defined as well as the point of view and the
focal length. *)

(** {1 Camera} *)

type camera [@@deriving yojson]
(** Type represented the camera. *)

val create :
  ?ux:Vect.t ->
  ?uz:Vect.t ->
  ?ratio:float ->
  ?image_width:int ->
  camera_center:Pos.t ->
  focal_length:float ->
  unit ->
  camera
(** [create] builds a new camera (only a virtual one though) based on :
  {ul 
    {- the position of the camera [camera_center]}
    {- the direction it is looking at [uz]}
    {- its orientation [ux]. [ux] and [uz] must be orthogonal and will be normed
      by the function}
    {- the ratio of the image [ratio]}
    {- the width of the image [image_width]}
    {- the focal length [focal_length]}}  *)

val default : camera
(** Default camera: 
  {ul 
    {- [camera_center] is in (0, 0, 0)}
    {- [basis] is (1, 0, 0) and (0, 1, 0) and (0, 0, 1)}
    {- [ratio] is 16/9 with width 400}
    {- [focal_length] is 1.}}  *)

(** {2 Accessors} *)

val camera_center : camera -> Pos.t
val image_width : camera -> int
val image_height : camera -> int

(** {1 Viewport} *)

type viewport = {
  upper_left : Pos.t;
  pixel_delta_u : Vect.t;
  pixel_delta_v : Vect.t;
  viewport_width : int;
  viewport_height : int;
  pixel_size : float;
}
[@@deriving yojson]
(** Virtual viewport throught which the ray pass. It is located on the [uz] axis
of the camera, at a distance of [-focal_length] of the [camera_center]. Its orientation 
is aligned with [ux] and [uy]. It is defined by: 
{ul 
{- [upper_left] : position of the upper left pixel}
{- [pixel_delta_u] : vector between two pixels on the [ux] axis}
{- [pixel_delta_v] : vector between two pixels on the [uy] axis (oriented in the 
opposite direction of [uy])}
{- [viewport_width] and [viewport_height] : size of the viewport in the [ux] and 
[uy]. Same ratio then the camera. }} *)

val create_viewport : ?viewport_height:float -> camera -> viewport
(** [create_viewport] create a new viewport. A viewport is the matrix of positions 
such as the casted rays go from the camera position to each of this positions. 
Default [viewport_height] value is 2.*)

val create_subviewport :
  upper_left:int * int ->
  viewport_width:int ->
  viewport_height:int ->
  viewport ->
  viewport
(** [create_subviewport ~upper_left ~width ~height viewport] create a new viewport that is subpart of [viewport] with a size of ([width], [height]). [upper_left] is a pixel position. *)
