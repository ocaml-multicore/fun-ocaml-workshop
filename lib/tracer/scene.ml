type form =
  | Sphere of { centre : Pos.t; radius : float }
  | Plane of { normal : Vect.t; point : Pos.t }
[@@deriving yojson]

type obj = { form : form; material : Material.t } [@@deriving yojson]
type scene = obj list [@@deriving yojson]

let sphere centre radius = Sphere { centre; radius = Float.abs radius }
let plane normal point = Plane { normal = Vect.unit_vector normal; point }
let to_string scene = scene_to_yojson scene |> Yojson.Safe.to_string

let of_string yojson_scene : scene =
  scene_of_yojson @@ Yojson.Safe.from_string yojson_scene |> Result.get_ok
