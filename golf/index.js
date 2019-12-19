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

  const speed = 12;
  const ground = create_ground();
  const R = 0.04267 * 2;
  const P = Vec3(0, -10 + R, 0);
  
  {
    Game.world.ball = sphere(R).model(gl);
    Game.world.land = ground.model(gl);
    Game.world.positions = [P];
  
    Game.camera.center = P;
    Game.camera.position = P.add(Vec3(-3, 1, 0));
    Game.camera.up = Vec3(0, 1, 0);
  }

  window.addEventListener("keydown", async e => {
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
        if (e.shiftKey) {
          if (Game.top.up_shift_key) return;
          Game.top.up_shift_key = true;
          window.setTimeout(() => top_up_shift_key_tick(new Date().getTime()), 0);
        } else {
          if (Game.top.up_key) return;
          Game.top.up_key = true;
          window.setTimeout(() => top_up_key_tick(new Date().getTime()), 0);
        }
      }
    } else if (e.code === "ArrowDown") {
      if (Game.world.status === WORLD_STATUS.top) {
        if (e.shiftKey) {
          if (Game.top.down_shift_key) return;
          Game.top.down_shift_key = true;
          window.setTimeout(() => top_down_shift_key_tick(new Date().getTime()), 0);
        } else {
          if (Game.top.down_key) return;
          Game.top.down_key = true;
          window.setTimeout(() => top_down_key_tick(new Date().getTime()), 0);
        }
      }
    } else if (e.code === "Digit0") {
      if (Game.world.status === WORLD_STATUS.normal) {
        Game.world.status = WORLD_STATUS.top;

        const p = Game.world.positions.slice(-1)[0];
        Game.camera.center = p;
        Game.camera.up = Vec3(1, 0, 0).rotate(Vec3(0, 1, 0), Game.hit.angle);
        Game.camera.position = p.add(Vec3(0, 10, 0));

      } else if (Game.world.status === WORLD_STATUS.top) {
        Game.world.status = WORLD_STATUS.normal;

        const p = Game.world.positions.slice(-1)[0];
        Game.camera.center = p;
        Game.camera.up = Vec3(0, 1, 0);
        Game.camera.position = p.add(Vec3(-3, 1, 0).rotate(Vec3(0, 1, 0), Game.hit.angle));
      }
    } else if (e.code === "ShiftLeft" || e.code === "ShiftRight") {
      if (Game.top.up_key) {
        Game.top.up_key = false;
        Game.top.up_shift_key = true;
        window.setTimeout(() => top_up_shift_key_tick(new Date().getTime()), 0);
      }
      if (Game.top.down_key) {
        Game.top.down_key = false;
        Game.top.down_shift_key = true;
        window.setTimeout(() => top_down_shift_key_tick(new Date().getTime()), 0);
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
          window.setTimeout(() => top_up_key_tick(new Date().getTime()), 0);
        }
        if (Game.top.down_shift_key) {
          Game.top.down_shift_key = false;
          Game.top.down_key = true;
          window.setTimeout(() => top_down_key_tick(new Date().getTime()), 0);
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
      const positions = make_positions(v, h0, ground, {
        r: R,
        W: 0,
        D: Math.PI / 180 * 0,
      });
      func(positions, h0, resolve);
    });
  };

  const left_key_tick = prev => {
    if (!Game.hit.left_key) return;

    const now = new Date().getTime();
    const O = Game.world.positions.slice(-1)[0];

    const dist = xz_distance(O, Game.camera.center) + 1;
    Game.hit.angle += 120 * (now - prev) / 1000 / dist;
    
    camera();

    window.setTimeout(() => left_key_tick(now), 0);
  };

  const right_key_tick = prev => {
    if (!Game.hit.right_key) return;

    const now = new Date().getTime();
    const O = Game.world.positions.slice(-1)[0];

    const dist = xz_distance(O, Game.camera.center) + 1;
    Game.hit.angle -= 120 * (now - prev) / 1000 / dist;
    
    camera();

    window.setTimeout(() => right_key_tick(now), 0);
  };

  const camera = () => {
    const O = Game.world.positions.slice(-1)[0];
    
    if (Game.world.status === WORLD_STATUS.top) {
      Game.camera.up = Vec3(1, 0, 0).rotate(Vec3(0, 1, 0), Game.hit.angle);
        
      const a = Vec3(1, 0 ,0).rotate(Vec3(0, 1, 0), Game.hit.angle);
      const b = Game.camera.center.sub(O);
      
      if (a.dot(b) >= 0) {
        Game.camera.center = Vec3(xz_distance(O, Game.camera.center), 0, 0).rotate(Vec3(0, 1, 0), Game.hit.angle).add(O);
      } else {
        Game.camera.center = Vec3(-xz_distance(O, Game.camera.center), 0, 0).rotate(Vec3(0, 1, 0), Game.hit.angle).add(O);
      }
      const y = Game.camera.position.y - Game.camera.center.y;
      Game.camera.position = Game.camera.center.add(Vec3(0, y, 0));
    } else if (Game.world.status === WORLD_STATUS.normal) {
      Game.camera.position = Vec3(-3, 1, 0).rotate(Vec3(0, 1, 0), Game.hit.angle).add(O);
    }
  };

  const top_up_key_tick = prev => {
    if (Game.world.status !== WORLD_STATUS.top) return;
    if (!Game.top.up_key) return;

    const now = new Date().getTime();
    const move = Vec3((now - prev) / 30, 0, 0).rotate(Vec3(0, 1, 0), Game.hit.angle);

    Game.camera.position = Game.camera.position.add(move);
    Game.camera.center = Game.camera.center.add(move);

    window.setTimeout(() => top_up_key_tick(now), 0);
  };

  const top_down_key_tick = prev => {
    if (Game.world.status !== WORLD_STATUS.top) return;
    if (!Game.top.down_key) return;

    const now = new Date().getTime();
    const move = Vec3((now - prev) / 30, 0, 0).rotate(Vec3(0, 1, 0), Game.hit.angle);
    
    Game.camera.position = Game.camera.position.sub(move);
    Game.camera.center = Game.camera.center.sub(move);

    window.setTimeout(() => top_down_key_tick(now), 0);
  };

  const top_up_shift_key_tick = prev => {
    if (Game.world.status !== WORLD_STATUS.top) return;
    if (!Game.top.up_shift_key) return;

    const p = Game.world.positions.slice(-1)[0];
    const now = new Date().getTime();
    const move = (now - prev) / 10;
    const temp = Game.camera.position.sub(Vec3(0, move, 0));
    
    if (temp.y < p.y + 10) temp.y = p.y + 10;
    Game.camera.position = temp;

    window.setTimeout(() => top_up_shift_key_tick(now), 0);
  };
  
  const top_down_shift_key_tick = prev => {
    if (Game.world.status !== WORLD_STATUS.top) return;
    if (!Game.top.down_shift_key) return;

    const p = Game.world.positions.slice(-1)[0];
    const now = new Date().getTime();
    const move = (now - prev) / 10;
    const temp = Game.camera.position.add(Vec3(0, move, 0));
    
    if (temp.y > p.y + 1000) temp.y = p.y + 1000;
    Game.camera.position = temp;

    window.setTimeout(() => top_down_shift_key_tick(now), 0);
  };
  
  loop(gl, ctx, prg);
});
