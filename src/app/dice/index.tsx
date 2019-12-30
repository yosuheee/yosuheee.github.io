import * as React from "react";
import * as ReactDOM from "react-dom";
import { program } from "../../webgl";
import { V3 } from "../../math/geometry/index";
import { dice } from "./module";

const App = () => (
  <canvas id="canvas" width="320" height="320"></canvas>
);

ReactDOM.render(
  <App />,
  document.getElementById("container"),
);

window.addEventListener("DOMContentLoaded", () => {
  const cvs = document.getElementById("canvas") as HTMLCanvasElement;
  const gl = cvs.getContext("webgl");
  const prg = program(gl, vertex_source, fragment_source);

  gl.enable(gl.CULL_FACE);
  gl.enable(gl.DEPTH_TEST);

  const mo = dice().translate(-6, -6, 6).scale(1.2, 1.2, 1.2).model(gl);
  
  let count = 0;
  const tick = () => {
    window.requestAnimationFrame(tick);
    count++;

    gl.clearColor(1, 1, 1, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);

    mo.rotate(V3(0, 1, -1), count)
      .lookAt(V3(30, 30, 30), V3(0, 0, 0), V3(0, 1, 0))
      .perspective(45, gl.canvas.width / gl.canvas.height, 0.1, 100)
      .draw(gl, prg, { light: V3(1, 1, -1) });
  };

  tick();
});


const vertex_source: string = `
attribute vec3 position;
attribute vec3 color;
attribute vec3 normal;
uniform mat4 mvp_matrix;
uniform mat4 inv_matrix;
varying vec3 v_color;
varying vec3 v_normal;

void main(void) {
  v_color = color;
  v_normal = (inv_matrix * vec4(normal, 0.0)).xyz;
  gl_Position = mvp_matrix * vec4(position, 1.0);
}
`;

const fragment_source: string = `
precision mediump float;
uniform vec3 light;
varying vec3 v_color;
varying vec3 v_normal;

void main(void) {
  gl_FragColor = vec4(v_color, 1.0);
  gl_FragColor.rgb *= dot(light, v_normal) / 10.0 + 0.85;
}
`;
