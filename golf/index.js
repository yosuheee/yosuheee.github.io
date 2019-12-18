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
      if (h.y <= -100) {
        return;
      }
      Game.world.positions.push(h);
      Game.distance.xz = xz_distance(h0, h) / 0.9144;
      window.setTimeout(() => func(positions, h, callback), 0);
    };
    return new Promise(resolve => {
      const p = Game.bar.power * 2 / 100;
      const v = Vec3(
        -p * Math.cos(Math.PI / 180 * 30),
        p * Math.sin(Math.PI / 180 * 30),
        0
      );
      func(make_positions(v, h0, ground, {
        W: 0,
        D: Math.PI / 180 * 0,
      }), h0, resolve);
    });
  };

  loop(gl, ctx, prg);
});
