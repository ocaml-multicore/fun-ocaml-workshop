(** This module defines the camera part of the ray tracer configuration. This 
is where the size of the image is defined as well as the point of view and the
focal length. *)

(** {1 Camera} *)

type camera = {
  center : Pos.t;
  ux : Vect.t;
  uy : Vect.t;
  uz : Vect.t;
  image_width : int;
  image_height : int;
  vfov : float;
  defocus_disk_u : Vect.t;
  defocus_disk_v : Vect.t;
  focus_dist : float;
  defocus_angle : float;
  nsamples : int;
  max_depth : int;
}
[@@deriving yojson]
(** Type represented the camera. *)

val create :
  ?nsamples:int ->
  ?max_depth:int ->
  ?ratio:float ->
  ?image_width:int ->
  ?vfov:float ->
  ?defocus_angle:float ->
  ?focus_dist:float ->
  vup:Vect.t ->
  lookat:Pos.t ->
  lookfrom:Pos.t ->
  unit ->
  camera
(** [create] builds a new camera (only a virtual one though) based on :
  {ul 
    {- the position of the camera [lookfrom]}
    {- the camera is looking at the point [lookat]}
    {- [vup] is the up vector of the camera}
    {- the ratio of the image [ratio]}
    {- the width of the image [image_width]}
    {- the vertical view angle [vfov] }
    {- [nsamples] defines the number of sampled rays per pixel. By default, it is 1 
(no sampling). }
    {- [max_depth] defines the maximum number of bounces of a ray. }}  *)

val default : camera
(** Default camera: 
  {ul 
    {- [camera_center] is in (0, 0, 0)}
    {- [basis] is (1, 0, 0) and (0, 1, 0) and (0, 0, 1)}
    {- [ratio] is 16/9 with width 400}
    {- [focal_length] is 1.}}  *)

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

val create_viewport : camera -> viewport
(** [create_viewport] create a new viewport. A viewport is the matrix of positions 
such as the casted rays go from the camera position to each of this positions. *)

val create_subviewport :
  upper_left:int * int ->
  viewport_width:int ->
  viewport_height:int ->
  viewport ->
  viewport
(** [create_subviewport ~upper_left ~width ~height viewport] create a new viewport that is subpart of [viewport] with a size of ([width], [height]). [upper_left] is a pixel position. *)
