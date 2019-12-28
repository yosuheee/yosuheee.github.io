import { program } from "/lib/webgl.js";
import { sphere } from "/lib/polygon.js";
import { V3 } from "/lib/geometry.js";

const gouraud_vertex_source = `
attribute vec3 position;
attribute vec3 color;
attribute vec3 normal;
uniform vec3 light;
varying vec3 vColor;
void main(void) {
  float diffuse = dot(normal, light);
  vColor = color * vec3(diffuse);
  gl_Position = vec4(position, 1.0);
}
`;

const gouraud_fragment_source = `
precision mediump float;
varying vec3 vColor;
void main(void) {
  gl_FragColor = vec4(vColor, 1.0);
}
`;

const phong_vertex_source = `
attribute vec3 position;
attribute vec3 color;
attribute vec3 normal;
varying vec3 vColor;
varying vec3 vNormal;
void main(void) {
  vColor = color;
  vNormal = normal;
  gl_Position = vec4(position, 1.0);
}
`;

const phong_fragment_source = `
precision mediump float;
uniform vec3 light;
varying vec3 vColor;
varying vec3 vNormal;
void main(void) {
  float diffuse = dot(vNormal, light);
  gl_FragColor = vec4(vColor * vec3(diffuse), 1.0);
}
`;

const po = sphere(0.8, [1, 1, 1], 64, 64);

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas1").getContext("webgl");
  const prg = program(gl, gouraud_vertex_source, gouraud_fragment_source);

  gl.enable(gl.CULL_FACE);
  gl.enable(gl.DEPTH_TEST);

  po.model(gl).draw(gl, prg, { light: V3(1, 1, 1) });
});

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas2").getContext("webgl");
  const prg = program(gl, phong_vertex_source, phong_fragment_source);
  
  gl.enable(gl.CULL_FACE);
  gl.enable(gl.DEPTH_TEST);

  po.model(gl).draw(gl, prg, { light: V3(1, 1, 1) });
});
