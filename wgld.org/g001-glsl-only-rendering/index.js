import { program, uniform } from "/module/webgl.js";
import { rect } from "/module/polygon.js";

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
  vec2 p = (gl_FragCoord.xy * 2.0 - resolution) / max(resolution.x, resolution.y);
  vec2 color = (vec2(1.0) + p.xy) * 0.5;
  gl_FragColor = vec4(color, 0.0, 1.0);
}
`;

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas").getContext("webgl");
  const prg = program(gl, vertex_source, fragment_source);
  const mo = rect(2.0, 2.0).translate(-1.0, -1.0, 0).model(gl);
  
  const start = new Date().getTime();

  let mx = 0.5, my = 0.5;

  document.getElementById("canvas").addEventListener("mousemove", e => {
    mx = e.offsetX / gl.canvas.width;
    my = e.offsetY / gl.canvas.height;
  });

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
