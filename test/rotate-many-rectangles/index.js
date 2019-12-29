import { program, VERTEX_SOURCE, FRAGMENT_SOURCE } from "/module/webgl.js";
import { rect } from "/module/polygon.js";
import { V3 } from "/module/geometry.js";
import { range } from "/module/util.js";

const po = rect(1.6, 1.6).translate(-0.8, -0.8, 0);

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas5").getContext("webgl");
  const prg = program(gl, VERTEX_SOURCE, FRAGMENT_SOURCE);
  const mo = po.model(gl);

  const S = 32;
  const W = 640 / S;
  const H = 480 / S;
  const speeds = range(W * H).map(() => Math.floor(Math.random() * 6 + 1) / 3);

  let count = 0;
  const tick = () => {
    requestAnimationFrame(tick);
    count++;
    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);
    for (let i = 0; i < W; i++)
    for (let j = 0; j < H; j++) {
      const x = i * S;
      const y = j * S;
      gl.viewport(x, y, S, S);
      mo.rotate(V3(0, 1, 0), count * speeds[i * H + j])
        .draw(gl, prg, { light: V3(0, 0, 1) });
    }
    gl.flush();
  };
  tick();
});

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas4").getContext("webgl");
  const prg = program(gl, VERTEX_SOURCE, FRAGMENT_SOURCE);
  const mo = po.model(gl);

  let count = 0;
  const tick = () => {
    requestAnimationFrame(tick);
    count++;
    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);
    mo.lookAt(V3(0, 0, 3).rotate(V3(-1, 0, 0), count),
              V3(0, 0, 0),
              V3(0, 1, 0).rotate(V3(-1, 0, 0), count))
      .perspective(45, gl.canvas.width / gl.canvas.height, 0.1, 100)
      .draw(gl, prg, { light: V3(1, 1, 1) });
  };
  tick();
});

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas3").getContext("webgl");
  const prg = program(gl, VERTEX_SOURCE, FRAGMENT_SOURCE);
  const mo = po.model(gl);

  let count = 0;
  const tick = () => {
    requestAnimationFrame(tick);
    count++;
    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);
    mo.rotate(V3(0, 1, 0), count)
      .draw(gl, prg, { light: V3(0, 0, 1) });
  };
  tick();
});

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas2").getContext("webgl");
  const prg = program(gl, VERTEX_SOURCE, FRAGMENT_SOURCE);
  const mo = po.model(gl);

  let count = 0;
  const tick = () => {
    requestAnimationFrame(tick);
    count++;
    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);
    mo.rotate(V3(0, 1, 0), count)
      .draw(gl, prg, { light: V3(0, 1, 0) });
  };
  tick();
});

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas1").getContext("webgl");
  const prg = program(gl, VERTEX_SOURCE, FRAGMENT_SOURCE);
  const mo = po.model(gl);

  let count = 0;
  const tick = () => {
    requestAnimationFrame(tick);
    count++;
    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);
    mo.rotate(V3(0, 1, 0), count)
      .draw(gl, prg, { light: V3(1, 0, 0) });
  };
  tick();
});
