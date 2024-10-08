type status = Start | Resolved of string [@@deriving yojson]

type task = {
  scene : Ray_tracer.Scene.scene;
  camera : Ray_tracer.Camera.camera;
  viewport : Ray_tracer.Camera.viewport;
}
[@@deriving yojson]

type sub = { x : int; y : int; w : int; h : int } [@@deriving yojson]
type job = { task : task; seed : int; sub : sub } [@@deriving yojson]

type t =
  | Fresh
  | Update of {
      username : string;
      color : string;
      position : sub;
      status : status;
    }
[@@deriving yojson]

let of_string msg =
  let json = Yojson.Safe.from_string msg in
  match of_yojson json with Ok v -> v | Error _ -> failwith "invalid json"

let to_string msg =
  let msg = to_yojson msg in
  Yojson.Safe.to_string msg

type response_ = { rect : sub; result_seed : int; result : string }
[@@deriving yojson]

type response = response_ list [@@deriving yojson]
