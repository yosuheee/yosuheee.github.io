import { program } from "/module/webgl.js";
import { Triangles } from "/module/triangles.js";
import { V3, Mat4 } from "/module/math/geometry/index.js";

const vertex_source = `
attribute vec3 position;
uniform mat4 mvp_matrix;
void main(void) {
  gl_Position = mvp_matrix * vec4(position, 1.0);
}
`;
const fragment_source = `
void main(void) {
  gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
}
`;

const po = new Triangles(
  [
    { position: V3(0, 0, 0), color: [1, 1, 1] },
    { position: V3(1, 0, 0), color: [1, 1, 1] },
    { position: V3(0, 1, 0), color: [1, 1, 1] },
  ],
  [[ 0, 1, 2 ]]
);

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas1").getContext("webgl");
  const prg = program(gl, vertex_source, fragment_source);

  const m = Mat4.scale(100, 100, 100)
                .translate(-50, -50, 0)
                .rotate(V3(0, 0, 1), 45)
                .lookAt(V3(0, 0, 200), V3(0, 0, 0), V3(0, 1, 0))
                .perspective(45, 1, 0.1, 1000);
  
  gl.clearColor(0.9, 0.9, 0.9, 1);
  gl.clear(gl.COLOR_BUFFER_BIT);
  po.transform(m).model(gl).draw(gl, prg, { col_name: "", nor_name: "" });
});

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas2").getContext("webgl");
  const prg = program(gl, vertex_source, fragment_source);

  const mot = po.model(gl);
  const mo = mot.scale(100, 100, 100)
                .translate(-50, -50, 0)
                .rotate(V3(0, 0, 1), 45)
                .lookAt(V3(0, 0, 200), V3(0, 0, 0), V3(0, 1, 0))
                .perspective(45, 1, 0.1, 1000);

  gl.clearColor(0.9, 0.9, 0.9, 1);
  gl.clear(gl.COLOR_BUFFER_BIT);
  mo.draw(gl, prg, { col_name: "", nor_name: "" });
});
