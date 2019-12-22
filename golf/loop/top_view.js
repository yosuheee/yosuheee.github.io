import { Vec3 } from "/lib/geometry.js";
import { WORLD_STATUS } from "../game.js";
import { xz_distance } from "../module.js";

export function top_view_loop(Game, prev = new Date().getTime()) {
  const now = new Date().getTime();
  window.setTimeout(() => top_view_loop(Game, now), 0);

  if (Game.world.status !== WORLD_STATUS.top) return;

  const lateral_direction_angular_travel = (now, prev, position, camera) => {
    const d = xz_distance(position, camera) + 1;
    return 120 * (now - prev) / 1000 / d;
  };

  const lateral_direction = p => {
    Game.camera.up = Vec3(1, 0, 0).rotate(Vec3(0, 1, 0), Game.hit.angle);
    const a = Vec3(1, 0 ,0).rotate(Vec3(0, 1, 0), Game.hit.angle);
    const b = Game.camera.center.sub(p);
    const d = xz_distance(p, Game.camera.center) * (a.dot(b) >= 0 ? 1 : -1);
    Game.camera.center = Vec3(d, 0, 0).rotate(Vec3(0, 1, 0), Game.hit.angle).add(p);
    const y = Game.camera.position.y - Game.camera.center.y;
    Game.camera.position = Game.camera.center.add(Vec3(0, y, 0));
  };
  
  if (Game.hit.left_key) {
    const p = Game.world.positions.slice(-1)[0];
    const d = lateral_direction_angular_travel(now, prev, p, Game.camera.position);
    Game.hit.angle += d;
    lateral_direction(p);
  }
  
  if (Game.hit.right_key) {
    const p = Game.world.positions.slice(-1)[0];
    const d = lateral_direction_angular_travel(now, prev, p, Game.camera.position);
    Game.hit.angle -= d;
    lateral_direction(p);
  }
  
  if (Game.top.up_key) {
    const move = Vec3((now - prev) / 10, 0, 0).rotate(Vec3(0, 1, 0), Game.hit.angle);
    Game.camera.position = Game.camera.position.add(move);
    Game.camera.center = Game.camera.center.add(move);
  }

  if (Game.top.down_key) {
    const move = Vec3((now - prev) / 10, 0, 0).rotate(Vec3(0, 1, 0), Game.hit.angle);
    Game.camera.position = Game.camera.position.sub(move);
    Game.camera.center = Game.camera.center.sub(move);
  }

  if (Game.top.up_shift_key) {
    const move = (now - prev) / 5;
    const temp = Game.camera.position.sub(Vec3(0, move, 0));
    const p = Game.world.positions.slice(-1)[0];
    if (temp.y < p.y + 10) temp.y = p.y + 10;
    Game.camera.position = temp;
  }

  if (Game.top.down_shift_key) {
    const move = (now - prev) / 5;
    const temp = Game.camera.position.add(Vec3(0, move, 0));
    const p = Game.world.positions.slice(-1)[0];
    if (temp.y > p.y + 1000) temp.y = p.y + 1000;
    Game.camera.position = temp;
  }
}
