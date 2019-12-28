import { program, VERTEX_SOURCE, FRAGMENT_SOURCE } from "../../lib/webgl.js";
import { rect, cube } from "../../lib/polygon.js";
import { range } from "../../lib/util.js";
import { V3 } from "../../lib/geometry.js";

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas1").getContext("webgl");
  const prg = program(gl, VERTEX_SOURCE, FRAGMENT_SOURCE);

  gl.enable(gl.CULL_FACE);
  gl.enable(gl.DEPTH_TEST);
  gl.enable(gl.BLEND);
  gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);

  const mo = range(9).map(i => {
    return rect(0.4, 0.4, [1, 0, 0, (i + 0.1) * 0.1]).translate(-0.6 + (i % 3) * 0.4, -0.6 + Math.floor(i / 3) * 0.4, 0);
  }).reduce((a, c) => a.add(c)).model(gl);

  let count = 0;
  const tick = () => {
    requestAnimationFrame(tick);
    count++;
    if (count === 400) count = 0;
    const s = Math.abs(count - 200) / 200;
    gl.clearColor(s, s, s, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);
    mo.draw(gl, prg, { light: V3(0, 0, 1) });
  };
  tick();
});

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas2").getContext("webgl");
  const prg = program(gl, VERTEX_SOURCE, FRAGMENT_SOURCE);

  gl.enable(gl.CULL_FACE);
  gl.enable(gl.DEPTH_TEST);
  gl.enable(gl.BLEND);
  gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);

  const mo = cube(0.8, [1, 0, 0, 0.3]).translate(-0.4, -0.4, 0.4).model(gl);

  let count = 0;
  const tick = () => {
    requestAnimationFrame(tick);
    count++;
    if (count === 400) count = 0;
    const s = Math.abs(count - 200) / 200;
    gl.clearColor(s, s, s, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);
    mo.rotate(V3(1, 1, 1), 60).draw(gl, prg, { light: V3(0, 0, 1) });
  };
  tick();
});
