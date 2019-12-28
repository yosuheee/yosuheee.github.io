import { program, VERTEX_SOURCE, FRAGMENT_SOURCE } from "../../lib/webgl.js";
import { cube, quarter_cylinder_rect, one_eighth_sphere, rounded_corners_cube, sphere } from "../../lib/polygon.js";
import { V3 } from "../../lib/geometry.js";

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas7").getContext("webgl");
  const prg = program(gl, VERTEX_SOURCE, FRAGMENT_SOURCE);

  gl.enable(gl.DEPTH_TEST);

  const mo = sphere(0.6, [1, 1, 1]).model(gl);
  let count = 0;
  const tick = () => {
    requestAnimationFrame(tick);
    count++;
    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);
    mo.rotate(V3(1, 1, 1), count)
      .lookAt(V3(0, 0, 2.4), V3(0, 0, 0), V3(0, 1, 0))
      .perspective(45, 1.0, 0.1, 100)
      .draw(gl, prg, { light: V3(1, 1, 1) });
  };
  tick();
});

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas8").getContext("webgl");
  const prg = program(gl, VERTEX_SOURCE, FRAGMENT_SOURCE);

  gl.enable(gl.DEPTH_TEST);

  const mo = sphere(0.6, [1, 1, 1]).flatten().model(gl);
  let count = 0;
  const tick = () => {
    requestAnimationFrame(tick);
    count++;
    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);
    mo.rotate(V3(1, 1, 1), count)
      .lookAt(V3(0, 0, 2.4), V3(0, 0, 0), V3(0, 1, 0))
      .perspective(45, 1.0, 0.1, 100)
      .draw(gl, prg, { light: V3(1, 1, 1) });
  };
  tick();
});

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas6").getContext("webgl");
  const prg = program(gl, VERTEX_SOURCE, FRAGMENT_SOURCE);

  gl.enable(gl.DEPTH_TEST);

  const mo = one_eighth_sphere(0.6).reverse().model(gl);
  let count = 0;
  const tick = () => {
    requestAnimationFrame(tick);
    count++;
    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);
    mo.rotate(V3(1, 0, 0), count)
      .lookAt(V3(1.6, 1.6, 1.6), V3(0, 0, 0), V3(0, 1, 0))
      .perspective(45, 1.0, 0.1, 100)
      .draw(gl, prg, { light: V3(1, 1, 1) });
  };
  tick();
});

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas1").getContext("webgl");
  const prg = program(gl, VERTEX_SOURCE, FRAGMENT_SOURCE);

  gl.enable(gl.CULL_FACE);
  gl.enable(gl.DEPTH_TEST);

  const mo = cube(0.5).translate(-0.25, -0.25, 0.25).model(gl);
  let count = 0;
  const tick = () => {
    requestAnimationFrame(tick);
    count++;
    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);
    mo.rotate(V3(1, 1, 1), count)
      .draw(gl, prg, { light: V3(1, 0.5, 1) });
  };
  tick();
});

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas2").getContext("webgl");
  const prg = program(gl, VERTEX_SOURCE, FRAGMENT_SOURCE);

  gl.enable(gl.DEPTH_TEST);

  const mo = quarter_cylinder_rect(1.6, 0.3).translate(-0.8, 0, 0).model(gl);
  let count = 0;
  const tick = () => {
    requestAnimationFrame(tick);
    count++;
    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);
    mo.rotate(V3(1, 0, 0), count)
      .lookAt(V3(1.6, 1.6, 1.6), V3(0, 0, 0), V3(0, 1, 0))
      .perspective(45, 1.0, 0.1, 100)
      .draw(gl, prg, { light: V3(1, 1, 1) });
  };
  tick();
});

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas3").getContext("webgl");
  const prg = program(gl, VERTEX_SOURCE, FRAGMENT_SOURCE);

  gl.enable(gl.DEPTH_TEST);

  const mo = one_eighth_sphere(0.6).model(gl);
  let count = 0;
  const tick = () => {
    requestAnimationFrame(tick);
    count++;
    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);
    mo.rotate(V3(1, 0, 0), count)
      .lookAt(V3(1.6, 1.6, 1.6), V3(0, 0, 0), V3(0, 1, 0))
      .perspective(45, 1.0, 0.1, 100)
      .draw(gl, prg, { light: V3(1, 1, 1) });
  };
  tick();
});

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas4").getContext("webgl");
  const prg = program(gl, VERTEX_SOURCE, FRAGMENT_SOURCE);

  gl.enable(gl.CULL_FACE);
  gl.enable(gl.DEPTH_TEST);

  const mo = rounded_corners_cube(0.4, 0.05).translate(-0.25, -0.25, 0.25).model(gl);
  let count = 0;
  const tick = () => {
    requestAnimationFrame(tick);
    count++;
    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);
    mo.rotate(V3(1, 1, 1), count)
      .draw(gl, prg, { light: V3(1, 0.5, 1) });
  };
  tick();
});
