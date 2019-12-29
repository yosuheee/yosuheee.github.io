import { Env, BAR_STATUS } from "../game.js";

export function bar_loop(Game) {
  window.setTimeout(() => bar_loop(Game), 0);

  if (Game.bar.status !== BAR_STATUS.power_undecided &&
      Game.bar.status !== BAR_STATUS.power_decided) return;

  const now = new Date().getTime() - Game.bar.start;
  const speed = Env.bar.speed;
  Game.bar.current = (speed * 100 - Math.abs(now - speed * 110)) / speed;
  if (Game.bar.current < -10) {
    Game.bar.status = BAR_STATUS.initial;
  }
}
