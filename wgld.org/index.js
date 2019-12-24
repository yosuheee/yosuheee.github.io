import { program } from "../lib/webgl.js";
import { Mat4, Vec3 } from "../lib/geometry.js";

const vss = `
attribute vec3 position;
uniform mat4 matrix;
void main(void) {
  gl_Position = matrix * vec4(position, 1.0);
  gl_PointSize = 10.0;
}
`;

const fss = `
void main(void) {
  gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
}
`;

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas").getContext("webgl");
  const prg = program(gl, vss, fss);

  gl.enable(gl.DEPTH_TEST);
  {
    const position = [
      0, 0, 0,
      1, 0, 0,
      0, 1, 0,
      1, 1, 0,
    ];
    const vbo = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, vbo);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(position), gl.STATIC_DRAW);
    const loc = gl.getAttribLocation(prg, "position");
    gl.enableVertexAttribArray(loc);
    gl.vertexAttribPointer(loc, 3, gl.FLOAT, false, 0, 0);
  }
  {
    const exps = [
      Mat4.ortho(-2, 2, -2, 2, 0.1, 100),
      Mat4.lookAt(Vec3(0, 0, 10), Vec3(0, 0, 0), Vec3(0, 1, 0)),
    ];
    const mat = exps.reduce((a, c) => a.mul(c)).primitive();
    const loc = gl.getUniformLocation(prg, "matrix");
    gl.uniformMatrix4fv(loc, false, mat);
  }

  gl.clearColor(0.9, 0.9, 0.9, 1.0);
  gl.clear(gl.COLOR_BUFFER_BIT);
  gl.drawArrays(gl.POINTS, 0, 4);
});
