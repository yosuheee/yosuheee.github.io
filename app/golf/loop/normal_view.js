import { V3 } from "/module/math/geometry/index.js";
import { WORLD_STATUS } from "../game.js";
import { xz_distance } from "../module.js";

export function normal_view_loop(Game, prev = new Date().getTime()) {
  const now = new Date().getTime();
  window.setTimeout(() => normal_view_loop(Game, now), 0);

  if (Game.world.status !== WORLD_STATUS.normal) return;

  if (Game.hit.left_key) {
    const p = Game.world.positions.slice(-1)[0];
    const d = xz_distance(p, Game.camera.center) + 1;
    Game.hit.angle += 120 * (now - prev) / 1000 / d;
    Game.camera.position = V3(-3, 1, 0).rotate(V3(0, 1, 0), Game.hit.angle).add(p);
  }

  if (Game.hit.right_key) {
    const p = Game.world.positions.slice(-1)[0];
    const d = xz_distance(p, Game.camera.center) + 1;
    Game.hit.angle -= 120 * (now - prev) / 1000 / d;
    Game.camera.position = V3(-3, 1, 0).rotate(V3(0, 1, 0), Game.hit.angle).add(p);
  }
}
