type t = { xv : float; yv : float; zv : float }

let create xv yv zv = { xv; yv; zv }

let add v1 v2 =
  { xv = v1.xv +. v2.xv; yv = v1.yv +. v2.yv; zv = v1.zv +. v2.zv }

let subtract v1 v2 =
  { xv = v1.xv -. v2.xv; yv = v1.yv -. v2.yv; zv = v1.zv -. v2.zv }

let neg v = { xv = -.v.xv; yv = -.v.yv; zv = -.v.zv }
let scale t v = { xv = v.xv *. t; yv = v.yv *. t; zv = v.zv *. t }
let norm v = Utils.distance v.xv v.yv v.zv
let norm2 v = Utils.distance2 v.xv v.yv v.zv
let dot (u : t) (v : t) = (u.xv *. v.xv) +. (u.yv *. v.yv) +. (u.zv *. v.zv)

let cross (u : t) (v : t) =
  {
    xv = (u.yv *. v.zv) -. (u.zv *. v.yv);
    yv = (u.zv *. v.xv) -. (u.xv *. v.zv);
    zv = (u.xv *. v.yv) -. (u.yv *. v.xv);
  }

let unit_vector v =
  let norm = norm v in
  if Utils.close_enough_float norm 0. then
    failwith "Trying to make a unit vector with a null vector"
  else scale (1. /. norm) v

let change_of_basis vx vy vz =
  let vx = unit_vector vx in
  let vy = unit_vector vy in
  let vz = unit_vector vz in
  assert (dot vx vy = 0. && cross vx vy = vz);

  [|
    [| vx.xv; vy.xv; vz.xv |];
    [| vx.yv; vy.yv; vz.yv |];
    [| vx.zv; vy.zv; vz.zv |];
  |]

let mul_mat_vect m v =
  let x = (m.(0).(0) *. v.xv) +. (m.(0).(1) *. v.yv) +. (m.(0).(2) *. v.zv) in
  let y = (m.(1).(0) *. v.xv) +. (m.(1).(1) *. v.yv) +. (m.(1).(2) *. v.zv) in
  let z = (m.(2).(0) *. v.xv) +. (m.(2).(1) *. v.yv) +. (m.(2).(2) *. v.zv) in
  create x y z

let change_basis m p =
  let x = (m.(0).(0) *. p.xv) +. (m.(0).(1) *. p.yv) +. (m.(0).(2) *. p.zv) in
  let y = (m.(1).(0) *. p.xv) +. (m.(1).(1) *. p.yv) +. (m.(1).(2) *. p.zv) in
  let z = (m.(2).(0) *. p.xv) +. (m.(2).(1) *. p.yv) +. (m.(2).(2) *. p.zv) in
  create x y z

(* let test () =
   let vx = create 1. 0. 1. |> unit_vector in
   let vy = create 0. 1. 0. |> unit_vector in
   let vz = cross vx vy in
   let m = change_of_basis vx vy vz in
   let v1 = create 1. 0. 0. in
   let v1' = change_basis m v1 in
   let v2 = create 1. 1. 0. in
   let v2' = change_basis m v2 in
   assert (Float.abs (scalar v1' vx -. norm2 vx) <= Float.epsilon);
   assert (
     let sqrt2 = sqrt 2. /. 2. in
     Float.abs (scalar v2' (create sqrt2 1. sqrt2) -. norm2 v2') <= Float.epsilon) *)

let random ?(min = -1.) ?(max = 1.) () =
  {
    xv = Random.float (max -. min) +. min;
    yv = Random.float (max -. min) +. min;
    zv = Random.float (max -. min) +. min;
  }

let random_unit ?(min = -1.) ?(max = 1.) () = random ~min ~max () |> unit_vector
