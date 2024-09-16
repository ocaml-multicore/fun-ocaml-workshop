type client

val client :
  ?uri:string ->
  username:string ->
  < clock : float Eio.Time.clock_ty Eio.Time.clock
  ; net : [> `Generic ] Eio.Net.ty Eio.Net.t
  ; .. > ->
  client

include Actor_shared.S with type client := client
