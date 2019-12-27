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
    const thi = this.index.length > that.index.length ? this : that;
    const tha = this.index.length > that.index.length ? that : this;
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
      this.matrix = M4();
      this.r_matrix = M4();
    } else if (a instanceof Model) {
      const model = a;
      const matrix = b;
      const r_matrix = c;
      this.position = model.position;
      this.color = model.color;
      this.normal = model.normal;
      this.index = model.index;

      this.index_length = model.index_length;
      this.matrix = matrix;
      this.r_matrix = r_matrix;
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
      const loc = gl.getUniformLocation(prg, "r_matrix");
      gl.uniformMatrix4fv(loc, false, this.r_matrix.primitive());
    }
    {
      const loc = gl.getUniformLocation(prg, "light");
      gl.uniform3fv(loc, light.normalize().primitive());
    }
    gl.drawElements(gl.TRIANGLES, this.index_length, gl.UNSIGNED_SHORT, 0);
  }
  scale(...args) {
    return new Model(this, this.matrix.scale(...args), this.r_matrix);
  }
  rotate(...args) {
    return new Model(this, this.matrix.rotate(...args), this.r_matrix.rotate(...args));
  }
  translate(...args) {
    return new Model(this, this.matrix.translate(...args), this.r_matrix);
  }
  lookAt(...args) {
    return new Model(this, this.matrix.lookAt(...args), this.r_matrix);
  }
  ortho(...args) {
    return new Model(this, this.matrix.ortho(...args), this.r_matrix);
  }
  perspective(...args) {
    return new Model(this, this.matrix.perspective(...args), this.r_matrix);
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

export function sphere(r, c = [1, 1, 1], row = 16, col = 16) {
  const parts = one_eighth_sphere(r, c, row, col);
  const objects = [
    parts,
    parts.rotate(V3(1, 0, 0),  90),
    parts.rotate(V3(1, 0, 0), 180),
    parts.rotate(V3(1, 0, 0), 270),
    parts.rotate(V3(0, 1, 0), -90),
    parts.rotate(V3(0, 1, 0), -90).rotate(V3(1, 0, 0),  90),
    parts.rotate(V3(0, 1, 0), -90).rotate(V3(1, 0, 0), 180),
    parts.rotate(V3(0, 1, 0), -90).rotate(V3(1, 0, 0), 270),
  ];
  return objects.reduce((a, c) => a.add(c));
}

export function cube(w, c = [1, 1, 1]) {
  const r = rect(w, w, c);
  const rects = [
    r,
    r.rotate(V3(1, 0, 0), -90).translate(0, w, 0),
    r.rotate(V3(1, 0, 0),  90).translate(0, 0, -w),
    r.rotate(V3(0, 1, 0),  90).translate(w, 0, 0),
    r.rotate(V3(0, 1, 0), -90).translate(0, 0, -w),
    r.rotate(V3(0, 1, 0), 180).translate(w, 0, -w),
  ];
  return rects.reduce((a, c) => a.add(c));
}

export function quarter_cylinder_rect(h, r, c = [1, 1, 1], d = 32) {
  const data = [], index = [];
  for (let i = 0; i <= d; i++) {
    const a = Math.PI / 180 * 90 / d * i;
    const y = r * Math.cos(a);
    const z = r * Math.sin(a);
    data.push({ position: V3(0, y, z), color: c });
    data.push({ position: V3(h, y, z), color: c });
  }
  for (let i = 0; i < d; i++) {
    const j = i * 2;
    index.push([j, j + 2, j + 1]);
    index.push([j + 2, j + 3, j + 1]);
  }
  return new Polygon(data, index);
}

export function one_eighth_sphere(r, c = [1, 1, 1], row = 32, col = 32) {
  const data = [], index = [];
  for (let i = 0; i <= row; i++) {
    const langle = Math.PI / 180 * 90 / row * i;
    const rad = r * Math.sin(langle);
    const y = r * Math.cos(langle);
    for (let j = 0; j <= col; j++) {
      const sangle = Math.PI / 180 * 90 / col * j;
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

export function rounded_corners_cube(w, r, c = [1, 1, 1]) {
  const face = rect(w, w, c);
  const border = quarter_cylinder_rect(w, r, c);
  const corner = one_eighth_sphere(r, c);

  const border4 = [
    border,
    border.rotate(V3(1, 0, 0),  90).translate(0, -w,  0),
    border.rotate(V3(1, 0, 0), 180).translate(0, -w, -w),
    border.rotate(V3(1, 0, 0), -90).translate(0,  0, -w),
  ].reduce((a, c) => a.add(c));

  const corner4 = [
    corner,
    corner.rotate(V3(1, 0, 0),  90).translate(0, -w,  0),
    corner.rotate(V3(1, 0, 0), 180).translate(0, -w, -w),
    corner.rotate(V3(1, 0, 0), -90).translate(0,  0, -w),
  ].reduce((a, c) => a.add(c));

  const objects = [
    face.translate(r, r, 0),
    face.rotate(V3(1, 0, 0), -90).translate(0, w,  0).translate(r, r * 2, -r),
    face.rotate(V3(1, 0, 0),  90).translate(0, 0, -w).translate(r, 0, -r),
    face.rotate(V3(0, 1, 0),  90).translate(w, 0,  0).translate(r * 2, r, -r),
    face.rotate(V3(0, 1, 0), -90).translate(0, 0, -w).translate(0, r, -r),
    face.rotate(V3(0, 1, 0), 180).translate(w, 0, -w).translate(r, r, -r * 2),
    border4.translate(r, w + r, -r),
    border4.rotate(V3(0, 1, 0), 90).translate(r + w, r + w, -r),
    border4.rotate(V3(0, 0, 1), 90).translate(r, r, -r),
    corner4.translate(w + r, w + r, -r),
    corner4.rotate(V3(0, 0, 1), 180).translate(r, r, -r),
  ];
  return objects.reduce((a, c) => a.add(c));
}
