import { program, VERTEX_SOURCE, FRAGMENT_SOURCE } from "/module/webgl.js";
import { rect } from "/module/polygon.js";

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas").getContext("webgl");
  const prg = program(gl, VERTEX_SOURCE, FRAGMENT_SOURCE);

  gl.enable(gl.CULL_FACE);
  gl.enable(gl.DEPTH_TEST);

  const mo = rect(1.6, 1.6).translate(-0.8, -0.8, 0).model(gl);
  gl.clearColor(0, 0, 0, 1);
  gl.clear(gl.COLOR_BUFFER_BIT);
  mo.draw(gl, prg);
});
