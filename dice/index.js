import { mat4 } from "/lib/gl-matrix/index.js";
import { program } from "/lib/webgl.js";
import { dice, uniformMatrix4, uniform3 } from "./module.js";

const vss = `
attribute vec3 position;
attribute vec3 color;
attribute vec3 normal;
uniform mat4 mvpMatrix;
uniform mat4 invMatrix;
varying vec3 vColor;
varying vec3 vNormal;
void main(void) {
  vColor = color;
  vNormal = mat3(invMatrix) * normal;
  gl_Position = mvpMatrix * vec4(position, 1.0);
}
`;

const fss = `
precision mediump float;
uniform vec3 reverseLightDirection;
varying vec3 vColor;
varying vec3 vNormal;
void main(void) {
  vec3 normal = normalize(vNormal);
  float light = dot(normal, normalize(reverseLightDirection));
  gl_FragColor = vec4(vColor, 1.0);
  gl_FragColor.rgb *= clamp(light * 2.0, 0.96, 0.99);
}
`;

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas").getContext("webgl");
  const prg = program(gl, vss, fss);
  gl.enable(gl.CULL_FACE);
  gl.enable(gl.DEPTH_TEST);

  const model = dice().translate(-6, -6, 6).model(gl);
  
  const vMatrix = mat4.create();
  const pMatrix = mat4.create();
  mat4.lookAt(vMatrix, [30, 15, 30], [0, 0, 0], [0, 1, 0]);
  mat4.perspective(pMatrix, 45, gl.canvas.width / gl.canvas.height, 0.1, 100);

  uniform3(gl, prg, "reverseLightDirection", [0.5, 0.7, 1.0]);

  let count = 0;
  const tick = () => {
    count++;
    {
      const mMatrix = mat4.create();
      mat4.fromRotation(mMatrix, Math.PI / 180 * count, [0, 1, -1]);
  
      const mvpMatrix = mat4.create();
      mat4.multiply(mvpMatrix, mMatrix, mvpMatrix);
      mat4.multiply(mvpMatrix, vMatrix, mvpMatrix);
      mat4.multiply(mvpMatrix, pMatrix, mvpMatrix);
  
      const invMatrix = mat4.create();
      mat4.invert(invMatrix, mMatrix);
      mat4.transpose(invMatrix, invMatrix);

      uniformMatrix4(gl, prg, "mvpMatrix", mvpMatrix);
      uniformMatrix4(gl, prg, "invMatrix", invMatrix);
    }
    gl.clearColor(0.88, 0.89, 0.88, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);

    model.prepare(gl, prg);
    gl.drawElements(gl.TRIANGLES, model.index.length, gl.UNSIGNED_SHORT, 0);
    gl.flush();

    window.requestAnimationFrame(tick);
  };

  tick();
});
