(** This module contains most of the core functions of this ray tracer. It 
defines rays, function to build them according to the camera configuration (
see {!build_rays}), as well as the functions computing intersections with a 
{!Scene.scene}.
*)

(** {1 Ray and rays} *)

type ray
(** Defined a ray. *)

type rays = ray array array
(** Defined a matrix of rays. This is actually the matrix of referent rays that 
go from the center of the camera (see {!section:Camera.camera}) to the center of each 
pixel of the viewport (see {!section:Camera.viewport }).

To avoid aliasing, this is actually not these rays that are used to check for 
intersection with the scene. Each referent ray are in fact computed by averaging 
[nsamples] rays (see {!rays_to_colors}) hitting the same viewport pixel that the 
referent ray but randomly offset from the center. *)

(** {2 Constructors} *)

val create : Vect.t -> Pos.t -> ray
(** [create dir origin] *)

val build_rays : Camera.camera -> Camera.viewport -> rays
(** [build_rays camera viewport] generates a matrix of rays that start at the origin 
of the [camera] and pass through each pixel of [viewport]. *)

(** {2 Accessors} *)

val dir : ray -> Vect.t
(** [dir ray] returns the direction of the ray. *)

val origin : ray -> Pos.t
(** [origin] returns the origin of the ray. A ray does not necessary originated 
from the camera as it can be a rebound (reflected or refracted) ray. *)

(** {1 Hit points} *)

type hit_point = {
  pos : Pos.t;
  obj : Scene.obj;
  front_face : bool;
  normal : Vect.t;
}
(** A hit point is the intersection point between a ray and an object. It is 
defined by:
{ul {- its position,}
    {- the object hit,}
    {- a boolean that defined which side of the object is hit, }
    {- the vector normal to the object at the hit position. The normal always 
    points outward the object. }} *)

(** {2 Intersection function }*)
val compute_hit_point : Scene.scene -> ray -> hit_point option
(** [compute_hit_point scene ray] returns the closest hit point of [ray] with 
one of the objects of [scene]. *)

(** {2 Color functions} *)

val ray_to_color :
  ?nsamples:int ->
  ?max_depth:int ->
  Scene.scene ->
  Camera.viewport ->
  ray ->
  Color.t
(** [ray_to_color ?progress_bar scene viewport ray] computes the colors of each ray.
{ul 
{- [nsamples] defines the number of sampled rays per pixel. By default, it is 1 
(no sampling). }
{- [max_depth] defines the maximum number of bounces of a ray. }}*)

val rays_to_colors :
  ?progress_bar:bool ->
  ?nsamples:int ->
  ?max_depth:int ->
  Scene.scene ->
  Pos.t ->
  Camera.viewport ->
  Color.t array array
(** [rays_to_colors ?progress_bar scene camera_center viewport] computes the colors of each ray.
{ul 
{- [nsamples] defines the number of sampled rays per pixel. By default, it is 1 
(no sampling). }
{- [max_depth] defines the maximum number of bounces of a ray. }}*)

val sky_color : ray -> Color.t
(** [sky_color ray] returns the color of the sky (no hitten objects) for the 
ray [ray]. *)
