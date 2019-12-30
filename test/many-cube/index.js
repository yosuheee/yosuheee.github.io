import { program, VERTEX_SOURCE, FRAGMENT_SOURCE } from "../../module/webgl.js";
import { rounded_corners_cube } from "../../module/triangles.js";
import { V3 } from "../../module/math/geometry/vec3.js";
import { range } from "../../module/util.js";
import { z_axis } from "../../module/math/geometry/index.js";

const A = [
  V3( 1,  0,  0),
  V3(-1,  0,  0),
  V3( 0,  1,  0),
  V3( 0, -1,  0),
  V3( 0,  0,  1),
  V3( 0,  0, -1),
];
const unit = 0.5;

window.addEventListener("DOMContentLoaded", () => {
  const cvs = document.getElementById("canvas");
  const gl = cvs.getContext("webgl");
  const prg = program(gl, VERTEX_SOURCE, FRAGMENT_SOURCE);
  const mo = rounded_corners_cube(0.4, 0.05).translate(-0.25, -0.25, 0).model(gl);

  gl.enable(gl.CULL_FACE);
  gl.enable(gl.DEPTH_TEST);

  const eye = V3(5, 0, 0);
  const up = V3(1, 1, 1).normalize();

  const to_i = (x, y, z) => x * 100 + y * 10 + z;
  const to_p = i => {
    const x = Math.floor(i / 100);
    const y = Math.floor((i - x * 100) / 10);
    const z = i - x * 100 - y * 10;
    return V3(x, y, z);
  };

  const per = Math.random() / 5 + 0.8;
  const position = range(1000).map(_ => []);
  for (let i = 0; i < 1000; i++) {
    if (Math.random() < per) continue;

    const [x, y, z] = to_p(i).primitive();

    position[i] = [(x - 4.5) * unit, (y - 4.5) * unit, (z - 4.5) * unit];
  }

  let count = 0;
  const tick = () => {
    requestAnimationFrame(tick);
    count++;

    if (Math.random() < per * 0.5) {
      const a = A[Math.floor(Math.random() * 6)];

      const i = Math.floor(Math.random() * 1000);
      const j = to_i(...to_p(i).add(a).primitive());
      if (i >= 0 && i < 1000 && j >= 0 && j < 1000 &&
          position[i].length === 3 && position[j].length === 0) {
        position[j] = to_p(j).primitive().map(d => (d - 4.5) * unit);
        position[i] = [];
      }
    }

    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);

    for (const [x, y, z] of position) {
      mo.translate(x, y, z)
        .lookAt(eye.rotate(z_axis, count / 3), V3(0, 0, 0), V3(0, 1, 0).rotate(z_axis, count / 3))
        .perspective(90, cvs.width / cvs.height, 0.1, 500)
        .draw(gl, prg);
    }
  };
  tick();
});
