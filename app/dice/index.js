import { V3 } from "/module/geometry.js";
import { program } from "/module/webgl.js";
import { dice } from "./module.js";

const vertex_source = `
attribute vec3 position;
attribute vec3 color;
attribute vec3 normal;
uniform mat4 mvp_matrix;
uniform mat4 inv_matrix;
varying vec3 v_color;
varying vec3 v_normal;

void main(void) {
  v_color = color;
  v_normal = (inv_matrix * vec4(normal, 0.0)).xyz;
  gl_Position = mvp_matrix * vec4(position, 1.0);
}
`;

const fragment_source = `
precision mediump float;
uniform vec3 light;
varying vec3 v_color;
varying vec3 v_normal;

void main(void) {
  gl_FragColor = vec4(v_color, 1.0);
  gl_FragColor.rgb *= dot(light, v_normal) / 10.0 + 0.85;
}
`;

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas").getContext("webgl");
  const prg = program(gl, vertex_source, fragment_source);

  gl.enable(gl.CULL_FACE);
  gl.enable(gl.DEPTH_TEST);

  const mo = dice().translate(-6, -6, 6).scale(1.2, 1.2, 1.2).model(gl);
  
  let count = 0;
  const tick = () => {
    window.requestAnimationFrame(tick);

    count++;

    gl.clearColor(1, 1, 1, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);

    mo.rotate(V3(0, 1, -1), count)
      .lookAt(V3(30, 30, 30), V3(0, 0, 0), V3(0, 1, 0))
      .perspective(45, gl.canvas.width / gl.canvas.height, 0.1, 100)
      .draw(gl, prg, { light: V3(1, 1, -1) });
  };

  tick();
});
