type status = Start | Resolved of string [@@deriving yojson]
type sub = { x : int; y : int; w : int; h : int } [@@deriving yojson]

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

type response = { sub : sub; result : string } [@@deriving yojson]
