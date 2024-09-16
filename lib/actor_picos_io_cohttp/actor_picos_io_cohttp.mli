type client

val client : ?uri:string -> username:string -> unit -> client
(** [client ~uri:"localhost:8080" ~username:"your-pseudo" ()] returns a [client] connected to server reachable at [uri]. *)

include Actor_shared.S with type client := client
(** @inline *)
