import { vec3, mat4 } from "/lib/gl-matrix/index.js";
import { Vec3 } from "/lib/geometry.js";

export class Polygon {
  constructor(data = [], index = []) {
    this.data = data;
    this.index = index;
  }
  add(poly) {
    const index = this.index.concat(
      poly.index.map(arr => arr.map(i => i + this.data.length))
    );
    const data = this.data.concat(poly.data);
    return new Polygon(data, index);
  }
  rotate(axis, angle) {
    return new Polygon(this.data.map(d => {
      const m = mat4.create();
      const p = vec3.create();
      mat4.fromRotation(m, Math.PI / 180 * angle, axis.primitive());
      vec3.transformMat4(p, d.position.primitive(), m);
      return { ...d, position: Vec3(p) };
    }), this.index);
  }
  translate(x, y, z) {
    return new Polygon(this.data.map(d => {
      const m = mat4.create();
      const p = vec3.create();
      mat4.fromTranslation(m, [x, y, z]);
      vec3.transformMat4(p, d.position.primitive(), m);
      return { ...d, position: Vec3(p) };
    }), this.index);
  }
  primitive() {
    const index = this.index.flat();
    const position = this.data.map(d => d.position.primitive()).flat();
    const color = this.data.map(d => d.color).flat();
    const normal = this.data.map(_ => Vec3(0, 0, 0));
    for (const [a, b, c] of this.index) {
      const A = this.data[a].position;
      const B = this.data[b].position;
      const C = this.data[c].position;
      const n = B.sub(A).cross(C.sub(A)).normalize();
      normal[a] = normal[a].add(n).normalize();
      normal[b] = normal[b].add(n).normalize();
      normal[c] = normal[c].add(n).normalize();
    }
    return {
      index,
      position,
      color,
      normal: normal.map(n => n.primitive()).flat(),
    };
  }
  model(gl) {
    return new Model(gl, this);
  }
  *triangles() {
    for (let i = 0; i < this.index.length; i++) {
      yield this.triangle(i).concat(i);
    }
  }
  triangle(i) {
    return this.index[i].map(p => this.data[p].position);
  }
}

class Model {
  constructor(a, b, c) {
    if (a instanceof WebGLRenderingContext) {
      const gl = a;
      const polygon = b;
      const { position, color, normal, index } = polygon.primitive();

      this.position = gl.createBuffer();
      gl.bindBuffer(gl.ARRAY_BUFFER, this.position);
      gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(position), gl.STATIC_DRAW);
      this.color = gl.createBuffer();
      gl.bindBuffer(gl.ARRAY_BUFFER, this.color);
      gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(color), gl.STATIC_DRAW);
      this.normal = gl.createBuffer();
      gl.bindBuffer(gl.ARRAY_BUFFER, this.normal);
      gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(normal), gl.STATIC_DRAW);
      this.index = gl.createBuffer();
      gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, this.index);
      gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, new Int16Array(index), gl.STATIC_DRAW);
  
      this.index_length = index.length;
      this.matrix = mat4.create();
    } else if (a instanceof Model) {
      const model = a;
      const matrix = b;
      this.position = model.position;
      this.color = model.color;
      this.normal = model.normal;
      this.index = model.index;

      this.index_length = model.index_length;
      this.matrix = matrix;
    } else {
      throw new Error("Unknown arguments");
    }
  }
  draw(gl, prg) {
    const list = [
      ["position", this.position],
      ["color", this.color],
      ["normal", this.normal],
    ];
    list.forEach(([name, vbo]) => {
      gl.bindBuffer(gl.ARRAY_BUFFER, vbo);
      const loc = gl.getAttribLocation(prg, name);
      gl.enableVertexAttribArray(loc);
      gl.vertexAttribPointer(loc, 3, gl.FLOAT, false, 0, 0);
    });
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, this.index);
    {
      const loc = gl.getUniformLocation(prg, "matrix");
      gl.uniformMatrix4fv(loc, false, this.matrix);
    }
    {
      const dir = Vec3(1, 1, -1);
      const loc = gl.getUniformLocation(prg, "light");
      gl.uniform3fv(loc, dir.normalize().primitive());
    }
    gl.drawElements(gl.TRIANGLES, this.index_length, gl.UNSIGNED_SHORT, 0);
  }
  rotate(axis, angle) {
    const mat = mat4.create();
    const out = mat4.create();
    mat4.fromRotation(mat, Math.PI / 180 * angle, axis.primitive());
    mat4.multiply(out, mat, this.matrix);
    return new Model(this, out);
  }
  translate(v) {
    const mat = mat4.create();
    const out = mat4.create();
    mat4.fromTranslation(mat, v.primitive());
    mat4.multiply(out, mat, this.matrix);
    return new Model(this, out);
  }
  lookAt(eye, center, up) {
    const mat = mat4.create();
    const out = mat4.create();
    mat4.lookAt(mat, eye.primitive(), center.primitive(), up.primitive());
    mat4.multiply(out, mat, this.matrix);
    return new Model(this, out);
  }
  perspective(fovy, aspect, near, far) {
    const mat = mat4.create();
    const out = mat4.create();
    mat4.perspective(mat, fovy, aspect, near, far);
    mat4.multiply(out, mat, this.matrix);
    return new Model(this, out);
  }
}

export function program(gl, vss, fss) {
  const prg = gl.createProgram();
  const list = [
    { type: gl.VERTEX_SHADER, source: vss },
    { type: gl.FRAGMENT_SHADER, source: fss },
  ];
  list.forEach(({ type, source }) => {
    const shader = gl.createShader(type);
    gl.shaderSource(shader, source);
    gl.compileShader(shader);
    gl.attachShader(prg, shader);
  });
  gl.linkProgram(prg);
  gl.useProgram(prg);
  return prg;
}

export const VERTEX_SOURCE = `
attribute vec3 position;
attribute vec3 color;
attribute vec3 normal;
uniform mat4 matrix;
varying vec3 v_color;
varying vec3 v_normal;

void main(void) {
  v_color = color;
  v_normal = normalize(mat3(matrix) * normal);
  gl_Position = matrix * vec4(position, 1.0);
}
`;

export const FRAGMENT_SOURCE = `
precision mediump float;
uniform vec3 light;
varying vec3 v_color;
varying vec3 v_normal;

void main(void) {
  gl_FragColor = vec4(v_color, 1.0);
  gl_FragColor.rgb *= clamp(dot(light, v_normal) / 5.0, 0.0, 0.2) + 0.9;
}
`;
