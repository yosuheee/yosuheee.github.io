import { V3 } from "/module/geometry.js";
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
    Game.camera.position = h.add(V3(-3, 1, 0).rotate(V3(0, 1, 0), Game.hit.angle));

    window.setTimeout(() => func(positions, h, resolve), 0);
  };
  
  return new Promise(resolve => {
    const p = Game.bar.power * 2 / 100;
    const v0 = V3(
      p * Math.cos(Math.PI / 180 * 30) * Math.cos(Math.PI / 180 * Game.hit.angle),
      p * Math.sin(Math.PI / 180 * 30),
      p * Math.cos(Math.PI / 180 * 30) * Math.sin(Math.PI / 180 * Game.hit.angle) * -1,
    );
    const positions = make_positions(v0, h0, Game.world.stage, Game.world.qtree, {
      r: Env.ball.radius,
      wind_power: Game.world.wind_power,
      wind_angle: Game.world.wind_angle,
    });
    func(positions, h0, resolve);
  });
};
