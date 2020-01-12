import { $ } from "../../module/util.js";
import { context, program, clear } from "../../module/webgl.js";
import { rect } from "../../module/triangles.js";

const vss = `
attribute vec3 position;
void main(void) {
  mat4 ident = mat4(
    vec4(1.0, 0.0, 0.0, 0.0),
    vec4(0.0, 1.0, 0.0, 0.0),
    vec4(0.0, 0.0, 1.0, 0.0),
    vec4(0.0, 0.0, 0.0, 1.0)
  );
  gl_Position = ident * vec4(position, 2.0);
}
`;

const fss = `
void main(void) {
  gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
}
`;

$(() => {
  const gl = context("canvas");
  const prg = program(gl, vss, fss);
  const mo = rect(1, 1).translate(-0.5, -0.5, 0).model(gl);

  clear(gl, 0, 0, 0);
  mo.draw(gl, prg, { col_name: "", nor_name: "" });
});
