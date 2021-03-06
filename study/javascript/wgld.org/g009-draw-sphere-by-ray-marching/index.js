import { context, program, uniform } from "../../../../module/webgl.js";
import { rect } from "../../../../module/triangles.js";
import { fetcht } from "../../../../module/util.js";

const vertex_source = `
attribute vec3 position;
void main(void) {
  gl_Position = vec4(position, 1.0);
}
`;

window.addEventListener("DOMContentLoaded", async () => {
  const fragment_source = await fetcht("./source.frag");
  const gl = context("canvas");
  const prg = program(gl, vertex_source, fragment_source);
  const mo = rect(2.0, 2.0).translate(-1.0, -1.0, 0).model(gl);

  const start = new Date().getTime();

  let is_down = false;
  let mx = 0.5, my = 0.5;

  const change = e => {
    mx = e.offsetX / gl.canvas.width;
    my = e.offsetY / gl.canvas.height;
  };

  gl.canvas.addEventListener("mousedown", e => { is_down = true; change(e); });
  gl.canvas.addEventListener("mouseup", () => is_down = false);
  gl.canvas.addEventListener("mousemove", e => { if (is_down) change(e); });

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
