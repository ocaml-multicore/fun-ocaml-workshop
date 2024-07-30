open Ray_tracer

(* Not working as well as expected  XD *)
let compute_min zoom_value center max_index =
  assert (zoom_value <= max_index);
  let center_f = Float.of_int center +. 0.5 in
  let new_max_f = Float.of_int max_index /. Float.of_int zoom_value in
  let b = center_f -. (new_max_f /. 2.) in
  let index_min = Float.floor b in
  let index_max = index_min +. new_max_f in
  let off =
    (b -. index_min) *. Float.of_int zoom_value |> Float.floor |> Int.of_float
  in
  let index_min = index_min |> Int.of_float in
  let index_max = index_max |> Int.of_float in
  if index_min < 0 then (0, Int.of_float new_max_f, 0)
  else if index_max >= max_index then begin
    let index_min = index_min - (index_max - max_index + 1) in

    (index_min, max_index, 0)
  end
  else (index_min, index_max, off)

let zoom ~image ~zoom_value ?(center = true) (iiul, jjul) (iibr, jjbr) =
  let height = Array.length image in
  let width = Array.length image.(0) in
  let zoom_value =
    if zoom_value > min height width then min height width else zoom_value
  in
  let iimin, iimax, iioff =
    if center then compute_min zoom_value iiul width else (iiul, iibr, 0)
  in
  let jjmin, jjmax, jjoff =
    if center then compute_min zoom_value jjul height else (jjul, jjbr, 0)
  in
  ( (iimin, jjmin),
    (iimax, jjmax),
    Utils.map2Dij
      (fun ii jj _ ->
        image.(((jj + jjoff) / zoom_value) + jjmin).(((ii + iioff) / zoom_value)
                                                     + iimin))
      image )

let move_view ~width ~height ~image ~zoom_value ~ulcorner ~rbcorner dir =
  let iimin, jjmin = ulcorner in
  let iimax, jjmax = rbcorner in
  let step = min width height / 100 in
  let niimin, niimax, njjmin, njjmax =
    match dir with
    | `Left ->
        let niimin = max 0 (iimin - step) in
        let niimax = niimin + iimax - iimin in
        (niimin, niimax, jjmin, jjmax)
    | `Up ->
        let njjmin = max 0 (jjmin - step) in
        let njjmax = njjmin + (jjmax - jjmin) in
        (iimin, iimax, njjmin, njjmax)
    | `Right ->
        let niimax = min (width - 1) (iimax + step) in
        let niimin = niimax - (iimax - iimin) in
        (niimin, niimax, jjmin, jjmax)
    | `Down ->
        let njjmax = min (height - 1) (jjmax + step) in
        let njjmin = njjmax - (jjmax - jjmin) in
        (iimin, iimax, njjmin, njjmax)
    | `Nothing -> (iimin, iimax, jjmin, jjmax)
  in
  zoom ~image ~zoom_value ~center:false (niimin, njjmin) (niimax, njjmax)
