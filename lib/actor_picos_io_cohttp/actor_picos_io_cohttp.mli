type client

val client : username:string -> client

include Actor_shared.S with type client := client
