import { context, program, VERTEX_SOURCE, FRAGMENT_SOURCE } from "../../module/webgl.js";
import { rect } from "../../module/triangles.js";
import { V3 } from "../../module/math/geometry/vec3.js";

window.addEventListener("DOMContentLoaded", () => {
  const gl = context("canvas");
  const prg = program(gl, VERTEX_SOURCE, FRAGMENT_SOURCE);
  const mo = rect(1, 1).translate(-0.5, -0.5, 0).model(gl);

  gl.clearColor(0, 0, 0, 1);
  gl.clear(gl.COLOR_BUFFER_BIT);
  mo.draw(gl, prg, {light: V3(0, 0, 1)});
});
