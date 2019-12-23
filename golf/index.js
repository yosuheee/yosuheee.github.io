import { Vec3 } from "/lib/geometry.js";
import { VERTEX_SOURCE, FRAGMENT_SOURCE, program } from "/lib/webgl.js";
import { sphere } from "/lib/polygon.js";
import { sleep } from "/lib/util.js";
import { Game, Env, BAR_STATUS, DISTANCE_STATUS, WORLD_STATUS } from "./game.js";
import { make_random_stage, make_qtree, xyz_from_xz } from "./module.js";
import { calculate } from "./calculate.js";
import { top_view_loop } from "./loop/top_view.js";
import { normal_view_loop } from "./loop/normal_view.js";
import { bar_loop } from "./loop/bar.js";
import { animation_loop } from "./loop/animation.js";

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas").getContext("webgl");
  const ctx = document.getElementById("text").getContext("2d");
  const prg = program(gl, VERTEX_SOURCE, FRAGMENT_SOURCE);

  gl.enable(gl.CULL_FACE);
  gl.enable(gl.DEPTH_TEST);

  {
    const stage = make_random_stage();
    const qtree = make_qtree(stage);

    Game.world.stage = stage;
    Game.world.qtree = qtree;
  
    const p = xyz_from_xz(stage, qtree, 0, 0);

    Game.world.ball = sphere(Env.ball.radius).model(gl);
    Game.world.land = stage.model(gl);
    Game.world.positions = [p];
  
    Game.camera.center = p;
    Game.camera.position = p.add(Vec3(-3, 1, 0));
    Game.camera.up = Vec3(0, 1, 0);

    Game.world.wind_power = Math.floor(Math.random() * 9) + 1;
    Game.world.wind_angle = Math.random() * 360;
  }

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
          break;
  
        case BAR_STATUS.power_undecided:
          Game.bar.status = BAR_STATUS.power_decided;
          const now = new Date().getTime() - Game.bar.start;
          const speed = Env.bar.speed;
          const pow = (speed * 100 - Math.abs(now - speed * 110)) / speed;
          Game.bar.power = pow;
          break;
  
        case BAR_STATUS.power_decided:
          Game.world.status = WORLD_STATUS.animation;
          Game.bar.status = BAR_STATUS.hide;
          Game.distance.status = DISTANCE_STATUS.show;
  
          const ok = await calculate(Game);

          if (!ok) {
            await sleep(1000);

            const p = xyz_from_xz(Game.world.stage, Game.world.qtree, 0, 0);

            Game.world.positions = [p];
            Game.hit.angle = 0;

            Game.camera.center = p;
            Game.camera.up = Vec3(0, 1, 0);
            Game.camera.position = p.add(Vec3(-3, 1, 0).rotate(Vec3(0, 1, 0), Game.hit.angle));
          }
  
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

  top_view_loop(Game);
  normal_view_loop(Game);
  bar_loop(Game);
  animation_loop(Game, gl, ctx, prg);
});
