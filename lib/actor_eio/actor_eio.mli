type client

val client : net:_ Eio.Net.t -> username:string -> client

type job = Protocol.sub = { x : int; y : int; w : int; h : int }

val request : client -> job
val respond : client -> job -> Image.image -> unit

(* *)
val vsplit : job -> job * job
val hsplit : job -> job * job
