import { Vec3 } from "/lib/geometry.js";
import { VERTEX_SOURCE, FRAGMENT_SOURCE, program } from "/lib/webgl.js";
import { make_positions } from "./calculate.js";
import { Game, animation_loop, BAR_STATUS, DISTANCE_STATUS, WORLD_STATUS } from "./game.js";
import { sphere, create_ground, xz_distance, create_quad_tree } from "./module.js";
import { top_view_loop } from "./top_view.js";
import { normal_view_loop } from "./normal_view.js";
import { intersection_of_plane_and_line, triangle_contains_point } from "../lib/geometry.js";

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas").getContext("webgl");
  const ctx = document.getElementById("text").getContext("2d");
  const prg = program(gl, VERTEX_SOURCE, FRAGMENT_SOURCE);

  gl.enable(gl.CULL_FACE);
  gl.enable(gl.DEPTH_TEST);

  const speed = 12;
  const ground = create_ground().rotate([1, 0, 0], 0);
  const R = 0.04267 * 2;
  const qt = create_quad_tree(ground)

  const position_from_xz = (x, z) => {
    const arrs = qt.search(x, z, x, z);

    for (const arr of arrs) for (const i of arr) {
      const [A, B, C] = ground.triangle(i);
      const D = Vec3(x, 0, z);
      const E = Vec3(x, 1, z);

      const Q = intersection_of_plane_and_line(A, B, C, D, E);
      if (triangle_contains_point(A, B, C, Q)) {
        const n = B.sub(A).cross(C.sub(A)).normalize();
        return Vec3(x, Q.y + R / n.y, z);
      }
    }
  };

  const P = position_from_xz(0, 0);

  Game.world.ball = sphere(R).model(gl);
  Game.world.land = ground.model(gl);
  Game.world.positions = [P];
  Game.world.quad_tree = qt;

  Game.camera.center = P;
  Game.camera.position = P.add(Vec3(-3, 1, 0));
  Game.camera.up = Vec3(0, 1, 0);

  window.addEventListener("keydown", async e => {
    if (e.code === "Space") {
      if (Game.world.status === WORLD_STATUS.top) {
        Game.world.status = WORLD_STATUS.normal;
        Game.bar.status = BAR_STATUS.initial;

        const p = Game.world.positions.slice(-1)[0];
        Game.camera.center = p;
        Game.camera.up = Vec3(0, 1, 0);
        Game.camera.position = p.add(Vec3(-3, 1, 0).rotate(Vec3(0, 1, 0), Game.hit.angle));
      } else if (Game.world.status === WORLD_STATUS.normal) {
        switch (Game.bar.status) {
  
        case BAR_STATUS.initial:
          Game.bar.status = BAR_STATUS.power_undecided;
          Game.bar.start = new Date().getTime();
          bar_tick();
          break;
  
        case BAR_STATUS.power_undecided:
          Game.bar.status = BAR_STATUS.power_decided;
          const now = new Date().getTime() - Game.bar.start;
          const pow = (speed * 100 - Math.abs(now - speed * 110)) / speed;
          Game.bar.power = pow;
          break;
  
        case BAR_STATUS.power_decided:
          Game.world.status = WORLD_STATUS.animation;
          Game.bar.status = BAR_STATUS.hide;
          Game.distance.status = DISTANCE_STATUS.show;
  
          await calculate();
  
          Game.world.positions = Game.world.positions.slice(-1);
          Game.world.status = WORLD_STATUS.normal;
          Game.bar.status = BAR_STATUS.initial;
          Game.distance.status = DISTANCE_STATUS.hide;

          break;
        }
      }
    } else if (e.code === "ArrowLeft") {
      if (Game.world.status === WORLD_STATUS.normal ||
          Game.world.status === WORLD_STATUS.top) {
        Game.hit.left_key = true;
      }
    } else if (e.code === "ArrowRight") {
      if (Game.world.status === WORLD_STATUS.normal ||
          Game.world.status === WORLD_STATUS.top) {
        Game.hit.right_key = true;
      }
    } else if (e.code === "ArrowUp") {
      if (Game.world.status === WORLD_STATUS.top) {
        if (e.shiftKey) {
          Game.top.up_shift_key = true;
        } else {
          Game.top.up_key = true;
        }
      }
    } else if (e.code === "ArrowDown") {
      if (Game.world.status === WORLD_STATUS.top) {
        if (e.shiftKey) {
          Game.top.down_shift_key = true;
        } else {
          Game.top.down_key = true;
        }
      }
    } else if (e.code === "Digit0") {
      if (Game.world.status === WORLD_STATUS.normal) {
        if (Game.bar.status !== BAR_STATUS.initial) return;

        Game.world.status = WORLD_STATUS.top;
        Game.bar.status = BAR_STATUS.hide;

        const p = Game.world.positions.slice(-1)[0];
        Game.camera.center = p;
        Game.camera.up = Vec3(1, 0, 0).rotate(Vec3(0, 1, 0), Game.hit.angle);
        Game.camera.position = p.add(Vec3(0, 10, 0));

      } else if (Game.world.status === WORLD_STATUS.top) {
        Game.world.status = WORLD_STATUS.normal;
        Game.bar.status = BAR_STATUS.initial;

        const p = Game.world.positions.slice(-1)[0];
        Game.camera.center = p;
        Game.camera.up = Vec3(0, 1, 0);
        Game.camera.position = p.add(Vec3(-3, 1, 0).rotate(Vec3(0, 1, 0), Game.hit.angle));
      }
    } else if (e.code === "ShiftLeft" || e.code === "ShiftRight") {
      if (Game.top.up_key) {
        Game.top.up_key = false;
        Game.top.up_shift_key = true;
      }
      if (Game.top.down_key) {
        Game.top.down_key = false;
        Game.top.down_shift_key = true;
      }
    }
  });

  window.addEventListener("keyup", e => {
    switch (e.code) {
      case "ArrowLeft"  : Game.hit.left_key       = false; break;
      case "ArrowRight" : Game.hit.right_key      = false; break;
      case "ArrowUp"    : Game.top.up_key         = false;
                          Game.top.up_shift_key   = false; break;
      case "ArrowDown"  : Game.top.down_key       = false;
                          Game.top.down_shift_key = false; break;
      case "ShiftLeft"  :
      case "ShiftRight" :
        if (Game.top.up_shift_key) {
          Game.top.up_shift_key = false;
          Game.top.up_key = true;
        }
        if (Game.top.down_shift_key) {
          Game.top.down_shift_key = false;
          Game.top.down_key = true;
        }
        break;
    }
  });

  const bar_tick = () => {
    const now = new Date().getTime() - Game.bar.start;
    const pow = (speed * 100 - Math.abs(now - speed * 110)) / speed;
    if (pow < -10) {
      Game.bar.status = BAR_STATUS.initial;
      return;
    }
    Game.bar.current = pow;
    if (Game.bar.status === BAR_STATUS.power_undecided ||
        Game.bar.status === BAR_STATUS.power_decided) {
      window.setTimeout(bar_tick, 0);
    }
  };

  const calculate = async () => {
    const h0 = Game.world.positions.slice(-1)[0];

    const func = (positions, prev, resolve) => {
      const h = positions.next().value;
      if (h.equals(prev)) {
        resolve();
        return;
      }
      if (h.sub(prev).length() < 0.0001) {
        resolve();
        return;
      }
      Game.world.positions.push(h);
      Game.distance.xz = xz_distance(h0, h) / 0.9144;

      Game.camera.center = h;
      Game.camera.position = h.add(Vec3(-3, 1, 0).rotate(Vec3(0, 1, 0), Game.hit.angle));

      window.setTimeout(() => func(positions, h, resolve), 0);
    };
    
    return new Promise(resolve => {
      const p = Game.bar.power * 2 / 100;
      const xz = p * Math.cos(Math.PI / 180 * 30);
      const y  = p * Math.sin(Math.PI / 180 * 30);
      const v = Vec3(
        xz *  Math.cos(Math.PI / 180 * Game.hit.angle),
        y,
        xz * -Math.sin(Math.PI / 180 * Game.hit.angle),
      );
      const positions = make_positions(v, h0, ground, Game.world.quad_tree, {
        r: R,
        W: 0,
        D: Math.PI / 180 * 0,
      });
      func(positions, h0, resolve);
    });
  };

  top_view_loop(Game);
  normal_view_loop(Game);
  animation_loop(gl, ctx, prg);
});
