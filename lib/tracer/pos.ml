type t = { xp : float; yp : float; zp : float } [@@deriving yojson]

let create xp yp zp = { xp; yp; zp }

let vector a b =
  Vect.{ xv = b.xp -. a.xp; yv = b.yp -. a.yp; zv = b.zp -. a.zp }

let offset offset pos =
  let open Vect in
  {
    xp = pos.xp +. offset.xv;
    yp = pos.yp +. offset.yv;
    zp = pos.zp +. offset.zv;
  }

let dist a b = Utils.distance (b.xp -. a.xp) (b.yp -. a.yp) (b.zp -. a.zp)

let change_basis m p =
  let x = (m.(0).(0) *. p.xp) +. (m.(0).(1) *. p.yp) +. (m.(0).(2) *. p.zp) in
  let y = (m.(1).(0) *. p.xp) +. (m.(1).(1) *. p.yp) +. (m.(1).(2) *. p.zp) in
  let z = (m.(2).(0) *. p.xp) +. (m.(2).(1) *. p.yp) +. (m.(2).(2) *. p.zp) in
  create x y z

let origin = create 0. 0. 0.
