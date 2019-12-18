import { Vec3 } from "/lib/geometry.js";
import { VERTEX_SOURCE, FRAGMENT_SOURCE, program } from "/lib/webgl.js";
import { make_positions } from "./calculate.js";
import { Game, loop, BAR_STATUS, DISTANCE_STATUS, WORLD_STATUS } from "./game.js";
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
    console.log(e.code);
    if (e.code === "Space") {
      if (Game.world.status === WORLD_STATUS.normal) {
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
        if (Game.hit.left_key) return;
        Game.hit.left_key = true;
        window.setTimeout(() => left_key_tick(new Date().getTime()), 0);
      }
    } else if (e.code === "ArrowRight") {
      if (Game.world.status === WORLD_STATUS.normal ||
          Game.world.status === WORLD_STATUS.top) {
        if (Game.hit.right_key) return;
        Game.hit.right_key = true;
        window.setTimeout(() => right_key_tick(new Date().getTime()), 0);
      }
    } else if (e.code === "ArrowUp") {
      if (Game.world.status === WORLD_STATUS.top) {
        if (Game.top.up_key) return;
        Game.top.up_key = true;
        window.setTimeout(() => top_up_key_tick(new Date().getTime()), 0);
      }
    } else if (e.code === "ArrowDown") {
      if (Game.world.status === WORLD_STATUS.top) {
        if (Game.top.down_key) return;
        Game.top.down_key = true;
        window.setTimeout(() => top_down_key_tick(new Date().getTime()), 0);
      }
    } else if (e.code === "Digit0") {
      if (Game.world.status === WORLD_STATUS.normal) {
        Game.world.status = WORLD_STATUS.top;
      } else if (Game.world.status === WORLD_STATUS.top) {
        Game.world.status = WORLD_STATUS.normal;
        const p = Game.world.positions.slice(-1)[0];
        Game.world.camera_center = p;
        Game.world.camera = p.add(
          Vec3(-3, 1, 0).rotate(Vec3(1, 0, 0), Game.hit.angle));
      }
    }
  });

  window.addEventListener("keyup", e => {
    if (e.code === "ArrowLeft") {
      if (Game.hit.left_key) {
        Game.hit.left_key = false;
      }
    } else if (e.code === "ArrowRight") {
      if (Game.hit.right_key) {
        Game.hit.right_key = false;
      }
    } else if (e.code === "ArrowUp") {
      if (Game.top.up_key) {
        Game.top.up_key = false;
      }
    } else if (e.code === "ArrowDown") {
      if (Game.top.down_key) {
        Game.top.down_key = false;
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

  const left_key_tick = prev => {
    if (!Game.hit.left_key) return;
    const now = new Date().getTime();
    Game.hit.angle += (now - prev) / key_interval;
    const p = Game.world.positions.slice(-1)[0];
    Game.world.camera_center = Vec3(xz_distance(Game.world.camera_center, p), 0, 0)
                                .rotate(Vec3(0, 1, 0), Game.hit.angle).add(p);
    window.setTimeout(() => left_key_tick(now), 0);
  };

  const right_key_tick = prev => {
    if (!Game.hit.right_key) return;
    const now = new Date().getTime();
    Game.hit.angle -= (now - prev) / key_interval;
    const p = Game.world.positions.slice(-1)[0];
    Game.world.camera_center = Vec3(xz_distance(Game.world.camera_center, p), 0, 0)
                                .rotate(Vec3(0, 1, 0), Game.hit.angle).add(p);
    window.setTimeout(() => right_key_tick(now), 0);
  };

  const top_up_key_tick = prev => {
    if (!Game.top.up_key) return;
    const now = new Date().getTime();
    const move = Vec3((now - prev) / 100, 0, 0).rotate(Vec3(0, 1, 0), Game.hit.angle);
    Game.world.camera = Game.world.camera.add(move);
    Game.world.camera_center = Game.world.camera_center.add(move);
    window.setTimeout(() => top_up_key_tick(now), 0);
  };

  const top_down_key_tick = prev => {
    if (!Game.top.down_key) return;
    const now = new Date().getTime();
    const move = Vec3((now - prev) / 100, 0, 0).rotate(Vec3(0, 1, 0), Game.hit.angle);
    Game.world.camera = Game.world.camera.sub(move);
    Game.world.camera_center = Game.world.camera_center.sub(move);
    window.setTimeout(() => top_down_key_tick(now), 0);
  };

  loop(gl, ctx, prg);
});
