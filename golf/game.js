import { Vec3 } from "/lib/geometry.js";
import { display_bar, display_distance } from "./module.js";

export const BAR_STATUS = {
  initial: 0,
  power_undecided: 1,
  power_decided: 2,
  impact: 3,
  hide: 4,
};

export const DISTANCE_STATUS = {
  hide: 0,
  show: 1,
};

export const WORLD_STATUS = {
  normal: 0,
  animation: 1,
  top: 2,
};

export const Game = {
  bar: {
    status: BAR_STATUS.initial,
    start: new Date().getTime(),
    current: new Date().getTime(),
    power: -1,
  },
  distance: {
    status: DISTANCE_STATUS.hide,
    xz: 0,
  },
  world: {
    status: WORLD_STATUS.normal,
    models: [],
    camera: Vec3(-3, -9 + 0.04267, 0),
    camera_center: Vec3(0, -10 + 0.04267, 0),
    sky_color: [160 / 255, 216 / 255, 239 / 255],
    positions: [Vec3(0, 0, 0)],
    ball: null,
    land: null,
  },
  hit: {
    angle: 0,
    left_key: false,
    right_key: false,
  },
  top: {
    up_key: false,
    down_key: false,
  },
};

export function loop(gl, ctx, prg) {
  requestAnimationFrame(() => loop(gl, ctx, prg));

  ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);

  switch (Game.bar.status) {
    case BAR_STATUS.initial:
      display_bar(ctx, -10, -10);
      break;
    case BAR_STATUS.power_undecided:
      display_bar(ctx, Game.bar.current, Game.bar.current);
      break;
    case BAR_STATUS.power_decided:
    case BAR_STATUS.impact:
      display_bar(ctx, Game.bar.power, Game.bar.current);
      break;
  }

  switch (Game.distance.status) {
    case DISTANCE_STATUS.show:
      display_distance(ctx, Game.distance.xz);
      break;
  }
  
  gl.clearColor(...Game.world.sky_color, 1);
  gl.clear(gl.COLOR_BUFFER_BIT);

  const p = Game.world.positions.slice(-1)[0];
  const models = [
    Game.world.land,
    Game.world.ball.translate(p),
  ];
  const direction = Vec3(-3, 1, 0).rotate(Vec3(0, 1, 0), Game.hit.angle);
  const camera = Game.world.status === WORLD_STATUS.top
                 ? Game.world.camera_center.add(Vec3(0, 10, 0))
                 : Game.world.camera_center.add(direction);
  const camera_up = Game.world.status === WORLD_STATUS.top
                    ? Vec3(1, 0, 0).rotate(Vec3(0, 1, 0), Game.hit.angle)
                    : Vec3(0, 1, 0);

  models.forEach(mo =>
    mo.lookAt(camera, Game.world.camera_center, camera_up)
      .perspective(45, gl.canvas.width / gl.canvas.height, 0.1, 1000)
      .draw(gl, prg));
}
