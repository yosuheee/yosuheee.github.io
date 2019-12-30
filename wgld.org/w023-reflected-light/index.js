import { program, uniform } from "/module/webgl.js";
import { sphere } from "/module/triangles.js";
import { V3 } from "/module/geometry.js";

const po = sphere(0.8, [0.6, 0.6, 0.6], 32, 32);

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas1").getContext("webgl");
  const prg = program(gl, vertex_source1, fragment_source1);

  gl.enable(gl.CULL_FACE);
  gl.enable(gl.DEPTH_TEST);

  const mo = po.model(gl);

  uniform(gl, prg, "vec4", "ambient_color", [0.1, 0.1, 0.1, 1.0]);

  let count = 0;
  const tick = () => {
    requestAnimationFrame(tick);
    count++;
    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);
    mo.rotate(V3(1, 1, 1), count).draw(gl, prg);
  };
  tick();
});

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas2").getContext("webgl");
  const prg = program(gl, vertex_source2, fragment_source2);

  gl.enable(gl.CULL_FACE);
  gl.enable(gl.DEPTH_TEST);

  const mo = po.flatten().model(gl);

  uniform(gl, prg, "vec4", "ambient_color", [0.1, 0.1, 0.1, 1.0]);
  uniform(gl, prg, "vec3", "eye_direction", V3(0, 0, 1).normalize().primitive());

  let count = 0;
  const tick = () => {
    requestAnimationFrame(tick);
    count++;
    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);
    mo.rotate(V3(1, 1, 1), count).draw(gl, prg);
  };
  tick();
});

const vertex_source1 = `
attribute vec3 position;
attribute vec4 color;
attribute vec3 normal;
uniform mat4 mvp_matrix;
varying vec4 v_color;
varying vec3 v_normal;

void main(void) {
  v_color = color;
  v_normal = normal;
  gl_Position = mvp_matrix * vec4(position, 1.0);
}
`;

const fragment_source1 = `
precision mediump float;
uniform vec3 light;
uniform vec4 ambient_color;
uniform vec3 eye_direction;
uniform mat4 inv_matrix;
varying vec4 v_color;
varying vec3 v_normal;

void main(void) {
  vec3 nor = (inv_matrix * vec4(v_normal, 0.0)).xyz;
  float diffuse = clamp(dot(nor, light), 0.0, 1.0);
  gl_FragColor = v_color * vec4(vec3(diffuse), 1.0) + ambient_color;
}
`;

const vertex_source2 = `
attribute vec3 position;
attribute vec4 color;
attribute vec3 normal;
uniform mat4 mvp_matrix;
varying vec4 v_color;
varying vec3 v_normal;

void main(void) {
  v_color = color;
  v_normal = normal;
  gl_Position = mvp_matrix * vec4(position, 1.0);
}
`;

const fragment_source2 = `
precision mediump float;
uniform vec3 light;
uniform vec4 ambient_color;
uniform vec3 eye_direction;
uniform mat4 inv_matrix;
varying vec4 v_color;
varying vec3 v_normal;

void main(void) {
  vec3 hal = normalize(light + eye_direction);
  vec3 nor = (inv_matrix * vec4(v_normal, 0.0)).xyz;
  float diffuse = clamp(dot(nor, light), 0.0, 1.0);
  float specular = pow(clamp(dot(nor, hal), 0.0, 1.0), 50.0);
  gl_FragColor = v_color * vec4(vec3(diffuse), 1.0) + vec4(vec3(specular), 1.0) + ambient_color;
}
`;
