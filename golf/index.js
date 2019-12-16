import { Vec3 } from "../lib/geometry.js";
import { VERTEX_SOURCE, FRAGMENT_SOURCE, program } from "../lib/webgl.js";
import { make_positions } from "./calculate.js";
import { sphere, 
         display_bar, create_ground, display_distance, xz_distance } from "./module.js";

window.addEventListener("load", () => {
  const gl = document.getElementById("canvas").getContext("webgl");
  const ctx = document.getElementById("text").getContext("2d");
  const prg = program(gl, VERTEX_SOURCE, FRAGMENT_SOURCE);

  gl.enable(gl.CULL_FACE);
  gl.enable(gl.DEPTH_TEST);

  const radius = 0.04267;
  const speed = 12;

  const ground = create_ground();
  const ball = sphere(radius).model(gl);
  const land = ground.model(gl);

  let h0 = Vec3(0, -10 + radius, 0);

  let status = 0;
  let hit = false;

  let start = new Date().getTime();
  let power = -1;

  window.addEventListener("keydown", e => {
    if (e.keyCode === 32) {
      switch (status) {
      case 0:
        status = 1;
        start = new Date().getTime();
        bar_tick();
        break;
      case 1:
        const now = new Date().getTime();
        const pow = (speed * 100 - Math.abs((now - start) - speed * 110)) / speed;
        power = pow;
        status = 2;
        break;
      case 2:
        status = 3;
        hit = true;
        const p = power * 2 / 100;
        const v = Vec3(
          -p * Math.cos(Math.PI / 180 * 30),
          p * Math.sin(Math.PI / 180 * 30),
          0
        );
        const positions = make_positions(v, h0, ground, {
          W: 0,
          D: Math.PI / 180 * 0,
        });

        scene_tick(positions, h0);
        break;
      }
    }
  });

  const bar_tick = () => {
    const now = new Date().getTime();
    const pow = (speed * 100 - Math.abs((now - start) - speed * 110)) / speed;
    if (pow < -10) {
      status = 0;
      power = -1;
      display_bar(ctx, -10, -10);
      return;
    }
    display_bar(ctx, power === -1 ? pow : power, pow);
    
    if (status >= 1 && status <= 2) {
      requestAnimationFrame(bar_tick);
    }
  };

  const scene_tick = (positions, _h) => {
    let stop = true;
    let p = _h;
    if (hit) {
      const [_, h] = positions.next().value;

      display_distance(ctx, xz_distance(h, h0) / 0.9144);

      if (!_h.equals(h)) stop = false;
      p = h;
    }

    const models = [
      land,
      ball.translate(p),
    ];

    gl.clearColor(160 / 255, 216 / 255, 239 / 255, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);

    models.forEach(mo =>
      mo.lookAt(p.add(Vec3(3, 2, 3)), p, Vec3(0, 1, 0))
        .perspective(45, gl.canvas.width / gl.canvas.height, 0.1, 500)
        .draw(gl, prg));

    if (!stop) {
      requestAnimationFrame(() => scene_tick(positions, p));
    } else {
      h0 = p;
      status = 0;
      power = -1;
      ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
      display_bar(ctx, -10, -10);
    }
  };

  scene_tick(null, h0);
});
