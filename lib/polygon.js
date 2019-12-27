import { V3, Mat4, M4 } from "./geometry.js";
import { range } from "./util.js";

export class Polygon {
  constructor(data = [], index = [], tridata = []) {
    this.data = data;
    this.index = index;
    this.tridata = tridata.length === 0 ?
      range(index.length).map(_ => {}) : tridata;
  }
  add(that) {
    let thi, tha;
    if (this.index.length > that.index.length) {
      thi = this, tha = that;
    } else {
      thi = that, tha = this;
    }
    const index = thi.index.concat(
      tha.index.map(arr => arr.map(i => i + thi.data.length))
    );
    const data = thi.data.concat(tha.data);
    const tridata = thi.tridata.concat(tha.tridata);
    return new Polygon(data, index, tridata);
  }
  scale(...args) {
    return this.transform(Mat4.scale(...args));
  }
  rotate(...args) {
    return this.transform(Mat4.rotate(...args));
  }
  translate(...args) {
    return this.transform(Mat4.translate(...args));
  }
  transform(m) {
    return new Polygon(this.data.map(d => {
      return { ...d, position: d.position.transform(m)};
    }), this.index, this.tridata);
  }
  primitive() {
    const index = this.index.flat();
    const position = this.data.map(d => d.position.primitive()).flat();
    const color = this.data.map(d => d.color).flat();
    const nlist = this.data.map(_ => []);
    for (const s of this.index) {
      const [A, B, C] = s.map(i => this.data[i].position);
      const n = B.sub(A).cross(C.sub(A)).normalize();
      s.forEach(i => nlist[i].push(n));
    }
    const normal = [];
    for (const arr of nlist) {
      const sum = arr.reduce((a, c) => a.add(c));
      normal.push(...sum.normalize().primitive());
    }
    return { index, position, color, normal };
  }
  model(gl) {
    return new Model(gl, this);
  }
  *triangles() {
    for (let i = 0; i < this.index.length; i++) {
      yield { ...this.triangle(i), index: i };
    }
  }
  triangle(i) {
    return {
      ...this.tridata[i],
      positions: this.index[i].map(p => this.data[p].position),
    };
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
      this.matrix = M4();
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
  draw(gl, prg, light = V3(1, 1, 1)) {
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
      gl.uniformMatrix4fv(loc, false, this.matrix.primitive());
    }
    {
      const loc = gl.getUniformLocation(prg, "light");
      gl.uniform3fv(loc, light.normalize().primitive());
    }
    gl.drawElements(gl.TRIANGLES, this.index_length, gl.UNSIGNED_SHORT, 0);
  }
  scale(...args) {
    return new Model(this, this.matrix.scale(...args));
  }
  rotate(...args) {
    return new Model(this, this.matrix.rotate(...args));
  }
  translate(...args) {
    return new Model(this, this.matrix.translate(...args));
  }
  lookAt(...args) {
    return new Model(this, this.matrix.lookAt(...args));
  }
  ortho(...args) {
    return new Model(this, this.matrix.ortho(...args));
  }
  perspective(...args) {
    return new Model(this, this.matrix.perspective(...args));
  }
}

export function rect(x, y, c = [1, 1, 1]) {
  const data = [], index = [];
  data.push({ position: V3(0, 0, 0), color: c });
  data.push({ position: V3(x, 0, 0), color: c });
  data.push({ position: V3(0, y, 0), color: c });
  data.push({ position: V3(x, y, 0), color: c });
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
      data.push({ position: V3(x, y, z), color: c });
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
