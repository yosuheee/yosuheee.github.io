import { program, uniform } from "/module/webgl.js";
import { rect } from "/module/triangles.js";

const vertex_source = `
attribute vec3 position;

void main(void) {
  gl_Position = vec4(position, 1.0);
}
`;

const fragment_source = `
precision mediump float;

uniform float time;
uniform vec2 mouse;
uniform vec2 resolution;

void main(void) {
  vec2 m = vec2(mouse.x * 2.0 - 1.0, -mouse.y * 2.0 + 1.0);
  vec2 p = (gl_FragCoord.xy * 2.0 - resolution) / max(resolution.x, resolution.y);
  float t = 0.1 / length(m - p);
  gl_FragColor = vec4(vec3(t), 1.0);
}
`;

window.addEventListener("DOMContentLoaded", () => {
  const cvs = document.getElementById("canvas");
  const gl = cvs.getContext("webgl");
  const prg = program(gl, vertex_source, fragment_source);
  const mo = rect(2.0, 2.0).translate(-1.0, -1.0, 0).model(gl);
  
  const start = new Date().getTime();

  let is_down = false;
  let mx = 0.5, my = 0.5;

  const change = e => {
    mx = e.offsetX / gl.canvas.width;
    my = e.offsetY / gl.canvas.height;
  };

  cvs.addEventListener("mousedown", e => { is_down = true; change(e); });
  cvs.addEventListener("mouseup", () => is_down = false);
  cvs.addEventListener("mousemove", e => { if (is_down) change(e); });

  const render = () => {
    requestAnimationFrame(render);

    const time = (new Date().getTime() - start) * 0.001;

    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);

    uniform(gl, prg, "float", "time", time);
    uniform(gl, prg, "vec2", "mouse", [mx, my]);
    uniform(gl, prg, "vec2", "resolution", [gl.canvas.width, gl.canvas.height]);

    mo.draw(gl, prg, { col_name: "", nor_name: "" });
  };

  render();
});
