import { Vec3 } from "/lib/geometry.js";
import { VERTEX_SOURCE, FRAGMENT_SOURCE, program } from "/lib/webgl.js";
import { make_positions } from "./calculate.js";
import { Game, loop, BAR_STATUS, DISTANCE_STATUS } from "./game.js";
import { sphere, create_ground, xz_distance } from "./module.js";

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas").getContext("webgl");
  const ctx = document.getElementById("text").getContext("2d");
  const prg = program(gl, VERTEX_SOURCE, FRAGMENT_SOURCE);

  gl.enable(gl.CULL_FACE);
  gl.enable(gl.DEPTH_TEST);

  const key_interval = 20;
  const speed = 12;
  const ground = create_ground();

  Game.world.ball = sphere(0.04267).model(gl);
  Game.world.land = ground.model(gl);
  Game.world.positions = [Vec3(0, -10 + 0.04267, 0)];

  window.addEventListener("keydown", async e => {
    if (e.keyCode === 32) {
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
        Game.bar.status = BAR_STATUS.hide;
        Game.distance.status = DISTANCE_STATUS.show;

        await calculate();

        Game.world.positions = Game.world.positions.slice(-1);
        Game.bar.status = BAR_STATUS.initial;
        Game.distance.status = DISTANCE_STATUS.hide;
        break;
      }
    } else if (e.keyCode === 37) {
      if (!Game.hit.left_key) {
        Game.hit.left_key = true;
        window.setTimeout(() => left_key_tick(new Date().getTime()), 0);
      }
    } else if (e.keyCode === 39) {
      if (!Game.hit.right_key) {
        Game.hit.right_key = true;
        window.setTimeout(() => right_key_tick(new Date().getTime()), 0);
      }
    }
  });

  window.addEventListener("keyup", e => {
    if (e.keyCode === 37) {
      if (Game.hit.left_key) {
        Game.hit.left_key = false;
      }
    } else if (e.keyCode === 39) {
      if (Game.hit.right_key) {
        Game.hit.right_key = false;
      }
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
    const func = (positions, prev, callback) => {
      const h = positions.next().value;
      if (h.equals(prev)) {
        callback();
        return;
      }
      Game.world.positions.push(h);
      Game.distance.xz = xz_distance(h0, h) / 0.9144;
      window.setTimeout(() => func(positions, h, callback), 0);
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
      const positions = make_positions(v, h0, ground, {
        W: 0,
        D: Math.PI / 180 * 0,
      });
      func(positions, h0, resolve);
    });
  };

  const left_key_tick = (prev) => {
    if (!Game.hit.left_key) return;
    const now = new Date().getTime();
    Game.hit.angle += (now - prev) / key_interval;
    window.setTimeout(() => left_key_tick(now), 0);
  };

  const right_key_tick = (prev) => {
    if (!Game.hit.right_key) return;
    const now = new Date().getTime();
    Game.hit.angle -= (now - prev) / key_interval;
    window.setTimeout(() => right_key_tick(now), 0);
  };

  loop(gl, ctx, prg);
});
