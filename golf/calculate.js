import { Vec3 } from "../lib/geometry.js";
import { Env } from "./game.js";
import { xz_distance } from "./module.js";
import { make_positions } from "./calculate_core.js";

export const calculate = async Game => {
  const h0 = Game.world.positions.slice(-1)[0];

  const func = (positions, prev, resolve) => {
    const h = positions.next().value;
    if (h.equals(prev)) {
      resolve(true);
      return;
    }
    if (h.sub(prev).length() < 0.001) {
      resolve(true);
      return;
    }
    if (h.y < -300) {
      resolve(false);
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
    const positions = make_positions(v, h0, Game.world.stage, Game.world.qtree, {
      r: Env.ball.radius,
      W: 0,
      D: Math.PI / 180 * 0,
    });
    func(positions, h0, resolve);
  });
};
