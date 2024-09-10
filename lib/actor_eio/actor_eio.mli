type client

val client :
  < clock : float Eio.Time.clock_ty Eio.Time.clock
  ; net : [> `Generic ] Eio.Net.ty Eio.Net.t
  ; .. > ->
  username:string ->
  client

include Actor_shared.S with type client := client
