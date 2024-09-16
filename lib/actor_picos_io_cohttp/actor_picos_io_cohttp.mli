type client

val client : ?uri:string -> username:string -> unit -> client

include Actor_shared.S with type client := client
