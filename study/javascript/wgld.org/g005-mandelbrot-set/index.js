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

vec3 hsv(float h, float s, float v) {
  vec4 t = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
  vec3 p = abs(fract(vec3(h) + t.xyz) * 6.0 - vec3(t.w));
  return v * mix(vec3(t.x), clamp(p - vec3(t.x), 0.0, 1.0), s);
}

void main(void) {
  vec2 m = vec2(mouse.x * 2.0 - 1.0, -mouse.y * 2.0 + 1.0);
  vec2 p = (gl_FragCoord.xy * 2.0 - resolution) / min(resolution.x, resolution.y);

  int j = 0;
  vec2  x = p + vec2(-0.5, 0.0);
  float y = 1.5 - mouse.x * 0.5;
  vec2  z = vec2(0.0, 0.0);
  for (int i = 0; i < 360; i++) {
    j++;
    if (length(z) > 2.0) { break; }
    z = vec2(z.x * z.x - z.y * z.y, 2.0 * z.x * z.y) + x * y;
  }
  float h = mod(time * 20.0, 360.0) / 360.0;
  vec3 rgb = hsv(h, 1.0, 1.0);
  float t = float(j) / 360.0;

  gl_FragColor = vec4(rgb * t, 1.0);
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
