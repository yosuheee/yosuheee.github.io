import { Vec3 } from "/lib/geometry.js";
import { program, VERTEX_SOURCE, FRAGMENT_SOURCE } from "/lib/webgl.js";
import { dice } from "./module.js";

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas").getContext("webgl");
  const prg = program(gl, VERTEX_SOURCE, FRAGMENT_SOURCE);

  gl.enable(gl.CULL_FACE);
  gl.enable(gl.DEPTH_TEST);

  const mo = dice().translate(-6, -6, 6).scale(Vec3(0.7, 0.7, 0.7)).model(gl);
  
  let count = 0;
  const tick = () => {
    window.requestAnimationFrame(tick);

    count++;

    gl.clearColor(0.57, 0.75, 0.61, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);

    mo.rotate(Vec3(0, 1, -1), count)
      .lookAt(Vec3(30, 30, 30), Vec3(0, 0, 0), Vec3(0, 1, 0))
      .perspective(45, gl.canvas.width / gl.canvas.height, 0.1, 100)
      .draw(gl, prg);
  };

  tick();
});
