import { display_bar, display_distance } from "../module.js";
import { BAR_STATUS, DISTANCE_STATUS } from "../game.js";

export function animation_loop(Game, gl, ctx, prg) {
  requestAnimationFrame(() => animation_loop(Game, gl, ctx, prg));

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

  models.forEach(mo =>
    mo.lookAt(Game.camera.position, Game.camera.center, Game.camera.up)
      .perspective(45, gl.canvas.width / gl.canvas.height, 0.01, 1200)
      .draw(gl, prg));
}
