import { program } from "/lib/webgl.js";
import { rect } from "/lib/polygon.js";
import { Mat4, V3 } from "/lib/geometry.js";
import { range } from "/lib/util.js";

const vertex_source = `
attribute vec3 position;
attribute vec3 color;
attribute vec3 normal;
uniform mat4 matrix;
uniform mat4 rotate;
uniform vec3 light;
varying vec3 vColor;
void main(void) {
  vec3 nor = (rotate * vec4(normal, 0.0)).xyz;
  float diffuse = clamp(dot(nor, light), 0.05, 1.0);
  vColor = color * vec3(diffuse);
  gl_Position = matrix * vec4(position, 1.0);
}
`;

const fragment_source = `
precision mediump float;
varying vec3 vColor;
void main(void) {
  gl_FragColor = vec4(vColor, 1.0);
}
`;

const po = rect(1.6, 1.6).translate(-0.8, -0.8, 0);

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas5").getContext("webgl");
  const prg = program(gl, vertex_source, fragment_source);
  const mo = po.model(gl);

  const S = 32;
  const W = 640 / S;
  const H = 480 / S;
  const speeds = range(W * H).map(() => Math.floor(Math.random() * 6 + 1) / 3);

  let count = 0;
  const tick = () => {
    requestAnimationFrame(tick);
    count++;
    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);
    for (let i = 0; i < W; i++)
    for (let j = 0; j < H; j++) {
      const x = i * S;
      const y = j * S;
      gl.viewport(x, y, S, S);
      {
        const rotate = Mat4.rotate(V3(0, 1, 0), count * speeds[i * H + j]);
        const location = gl.getUniformLocation(prg, "rotate");
        gl.uniformMatrix4fv(location, false, rotate.primitive());
      }
      mo.rotate(V3(0, 1, 0), count * speeds[i * H + j])
        .draw(gl, prg, V3(0, 0, 1));
    }
    gl.flush();
  };
  tick();
});

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas4").getContext("webgl");
  const prg = program(gl, vertex_source, fragment_source);
  const mo = po.model(gl);

  let count = 0;
  const tick = () => {
    requestAnimationFrame(tick);
    count++;
    {
      const m = Mat4.rotate(V3(0, 1, 0), 0);
      const l = gl.getUniformLocation(prg, "rotate");
      gl.uniformMatrix4fv(l, false, m.primitive());
    }
    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);
    mo.lookAt(V3(0, 0, 3).rotate(V3(-1, 0, 0), count),
              V3(0, 0, 0),
              V3(0, 1, 0).rotate(V3(-1, 0, 0), count))
      .perspective(45, gl.canvas.width / gl.canvas.height, 0.1, 100)
      .draw(gl, prg, V3(1, 1, 1));
  };
  tick();
});

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas3").getContext("webgl");
  const prg = program(gl, vertex_source, fragment_source);
  const mo = po.model(gl);

  let count = 0;
  const tick = () => {
    requestAnimationFrame(tick);
    count++;
    const m = Mat4.rotate(V3(0, 1, 0), count);
    {
      const loc = gl.getUniformLocation(prg, "rotate");
      gl.uniformMatrix4fv(loc, false, m.primitive());
    }

    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);
    mo.rotate(V3(0, 1, 0), count)
      .draw(gl, prg, V3(0, 0, 1));
  };
  tick();
});

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas2").getContext("webgl");
  const prg = program(gl, vertex_source, fragment_source);
  const mo = po.model(gl);

  let count = 0;
  const tick = () => {
    requestAnimationFrame(tick);
    count++;
    const m = Mat4.rotate(V3(0, 1, 0), count);
    {
      const loc = gl.getUniformLocation(prg, "rotate");
      gl.uniformMatrix4fv(loc, false, m.primitive());
    }

    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);
    mo.rotate(V3(0, 1, 0), count)
      .draw(gl, prg, V3(0, 1, 0));
  };
  tick();
});

window.addEventListener("DOMContentLoaded", () => {
  const gl = document.getElementById("canvas1").getContext("webgl");
  const prg = program(gl, vertex_source, fragment_source);
  const mo = po.model(gl);

  let count = 0;
  const tick = () => {
    requestAnimationFrame(tick);
    count++;
    const m = Mat4.rotate(V3(0, 1, 0), count);
    {
      const loc = gl.getUniformLocation(prg, "rotate");
      gl.uniformMatrix4fv(loc, false, m.primitive());
    }

    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);
    mo.rotate(V3(0, 1, 0), count)
      .draw(gl, prg, V3(1, 0, 0));
  };
  tick();
});
