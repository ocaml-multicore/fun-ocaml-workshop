type form =
  | Sphere of { centre : Pos.t; radius : float }
  | Plane of { normal : Vect.t; point : Pos.t }

type obj = { form : form; material : Material.t }
type scene = obj list

let sphere centre radius = Sphere { centre; radius = Float.abs radius }
let plane normal point = Plane { normal = Vect.unit_vector normal; point }
