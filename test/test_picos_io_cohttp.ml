open Picos_std_structured
module Actor = Actor_picos_io_cohttp

let n_domains = 4
let username = Sys.argv.(1)
let width_and_height = 1408

module Renderer = struct
  open Ray_tracer

  type t = {
    rays : Ray.rays;
    scene : Scene.scene;
    max_depth : int;
    viewport : Camera.viewport;
  }

  let create () =
    let camera =
      Camera.create ~image_width:width_and_height ~ratio:1.0
        ~camera_center:(Pos.create 0. 0. 0.) ~focal_length:1. ()
    in
    let viewport = Camera.create_viewport ~viewport_height:2.0 camera in
    let rays = Ray.build_rays camera viewport in
    let scene : Scene.scene =
      let open Scene in
      [
        {
          form = sphere (Pos.create 0. (-1000.5) (-1.)) 1000.;
          material = Material.create_lambertian (Color.rgb 0.5 0.5 0.5);
        };
        {
          form = Scene.sphere (Pos.create 0. 0. (-1.)) 0.5;
          material = Material.create_lambertian (Color.rgb 0.4 0.2 0.);
        };
      ]
    in
    { rays; scene; max_depth = 10; viewport }

  let render { rays; scene; max_depth; viewport } (sub : Protocol.sub) =
    let img = Image.create_rgb sub.w sub.h in

    for y = sub.y to sub.y + sub.h - 1 do
      for x = sub.x to sub.x + sub.w - 1 do
        let c = Ray.ray_to_color ~max_depth scene viewport rays.(y).(x) in
        Image.write_rgb img (x - sub.x) (y - sub.y)
          (Float.to_int (c.r *. 255.0))
          (Float.to_int (c.g *. 255.0))
          (Float.to_int (c.b *. 255.0))
      done
    done;

    img
end

(* *)

let main () =
  let renderer = Renderer.create () in

  Flock.join_after @@ fun () ->
  for _ = 1 to n_domains do
    Flock.fork @@ fun () ->
    let client = Actor.client ~username in

    while true do
      let sub = Actor.request client in
      let img = Renderer.render renderer sub in

      Actor.respond client sub img;

      Control.sleep ~seconds:(0.2 +. Random.float 0.03)
    done
  done

let () = Picos_mux_multififo.run_on ~n_domains main
