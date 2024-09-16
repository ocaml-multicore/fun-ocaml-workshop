type client

val client :
  ?uri:string ->
  username:string ->
  < clock : float Eio.Time.clock_ty Eio.Time.clock
  ; net : [> `Generic ] Eio.Net.ty Eio.Net.t
  ; .. > ->
  client
(** [client ~uri:"localhost:8080" ~username:"your-pseudo" env] returns a [client] connected to server reachable at [uri]. *)

include Actor_shared.S with type client := client
(** @inline *)
