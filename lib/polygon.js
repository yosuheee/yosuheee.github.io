import { vec3, mat4 } from "./gl-matrix/index.js";
import { Vec3 } from "./geometry.js";

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
  scale(v) {
    return new Polygon(this.data.map(d => {
      const m = mat4.create();
      const p = vec3.create();
      mat4.fromScaling(m, v.primitive());
      vec3.transformMat4(p, d.position.primitive(), m);
      return { ...d, position: Vec3(p) };
    }), this.index);
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
  constructor(a, b) {
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

export function rect(x, y, c = [1, 1, 1]) {
  const data = [], index = [];
  data.push({ position: Vec3(0, 0, 0), color: c });
  data.push({ position: Vec3(x, 0, 0), color: c });
  data.push({ position: Vec3(0, y, 0), color: c });
  data.push({ position: Vec3(x, y, 0), color: c });
  index.push([0, 1, 2]);
  index.push([2, 1, 3]);
  return new Polygon(data, index);
}

export function sphere(radius, c = [1, 1, 1], row = 32, col = 32) {
  const data = [], index = [];
  for (let i = 0; i <= row; i++) {
    const langle = Math.PI / row * i;
    const rad = radius * Math.sin(langle);
    const y = radius * Math.cos(langle);
    for (let j = 0; j <= col; j++) {
      const sangle = Math.PI * 2 / col * j;
      const x = rad * Math.cos(sangle);
      const z = rad * Math.sin(sangle);
      data.push({ position: Vec3(x, y, z), color: c });
    }
  }
  for (let i = 0; i < row; i++) {
    for (let j = 0; j < col; j++) {
      const std1 = i * (col + 1) + j;
      const std2 = (i + 1) * (col + 1) + j;
      index.push([std1, std1 + 1, std2]);
      index.push([std1 + 1, std2 + 1, std2]);
    }
  }
  return new Polygon(data, index);
}
