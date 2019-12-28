import { program, uniform } from "../../lib/webgl.js";
import { sphere } from "../../lib/polygon.js";
import { V3 } from "../../lib/geometry.js";

const vertex_source = `
attribute vec3 position;
attribute vec4 color;
attribute vec3 normal;
uniform vec3 light;
uniform mat4 m_matrix;
uniform mat4 r_matrix;
uniform vec4 ambient_color;
varying vec4 v_color;

void main(void) {
  float diffuse = clamp(dot(light, (r_matrix * vec4(normal, 0.0)).xyz), 0.0, 1.0);
  v_color = color * vec4(diffuse) + ambient_color;
  gl_Position = m_matrix * vec4(position, 1.0);
}
`;

const fragment_source = `
precision mediump float;
varying vec4 v_color;

void main(void) {
  gl_FragColor = v_color;
}
`;

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas1").getContext("webgl");
  const prg = program(gl, vertex_source, fragment_source);

  gl.enable(gl.CULL_FACE);
  gl.enable(gl.DEPTH_TEST);

  const mo = sphere(0.8).model(gl);

  uniform(gl, prg, "vec4", "ambient_color", [0.0, 0.0, 0.0, 1.0]);

  gl.clearColor(0, 0, 0, 1);
  gl.clear(gl.COLOR_BUFFER_BIT);
  mo.draw(gl, prg, { light: V3(1, 1, -1) });
});

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas2").getContext("webgl");
  const prg = program(gl, vertex_source, fragment_source);

  gl.enable(gl.CULL_FACE);
  gl.enable(gl.DEPTH_TEST);

  const mo = sphere(0.8).model(gl);

  uniform(gl, prg, "vec4", "ambient_color", [0.1, 0.1, 0.1, 1.0]);

  gl.clearColor(0, 0, 0, 1);
  gl.clear(gl.COLOR_BUFFER_BIT);
  mo.draw(gl, prg, { light: V3(1, 1, -1) });
});

window.addEventListener("DOMContentLoaded", () => {
  const range = document.getElementById("range");
  const gl = document.getElementById("canvas3").getContext("webgl");
  const prg = program(gl, vertex_source, fragment_source);
  
  gl.enable(gl.CULL_FACE);
  gl.enable(gl.DEPTH_TEST);

  const mo = sphere(0.8).model(gl);

  const draw = ambient_color => {
    uniform(gl, prg, "vec4", "ambient_color", ambient_color);

    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);
    mo.draw(gl, prg, { light: V3(1, 1, -1) });

    document.getElementById("range_value").textContent = (+range.value).toFixed(2);
  };

  const tick = () => {
    requestAnimationFrame(tick);
    const r = +range.value;
    draw([r, r, r, 1.0]);
  };
  tick();
});
