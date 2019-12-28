import { V3, Mat4, M4 } from "./geometry.js";
import { range } from "./util.js";
import { power } from "./math.js";
import { uniform, buffer } from "./webgl.js";
import { UnionFind } from "./union-find.js";

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
  reverse() {
    return new Polygon(this.data, this.index.map(i => [i[0], i[2], i[1]]), this.tridata);
  }
  primitive() {
    const index = this.index.flat();
    const position = this.data.map(d => d.position.primitive()).flat();
    const color = this.data.map(d => d.color.length === 3 ? d.color.concat([1.0]) : d.color).flat();
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
  flatten(eps = 0.001) {
    const N = 5;
    const n = this.data.length;
    
    const pos = this.data.map(d => d.position);
    const min = {
      x: pos.reduce((a, c) => Math.min(a, c.x), 1e9),
      y: pos.reduce((a, c) => Math.min(a, c.y), 1e9),
      z: pos.reduce((a, c) => Math.min(a, c.z), 1e9),
    };
    const max = {
      x: pos.reduce((a, c) => Math.max(a, c.x), -1e9) + 0.01,
      y: pos.reduce((a, c) => Math.max(a, c.y), -1e9) + 0.01,
      z: pos.reduce((a, c) => Math.max(a, c.z), -1e9) + 0.01,
    };
    const to_index = p => {
      const x = Math.floor((p.x - min.x) / (max.x - min.x) * power(2, N));
      const y = Math.floor((p.y - min.y) / (max.y - min.y) * power(2, N));
      const z = Math.floor((p.z - min.z) / (max.z - min.z) * power(2, N));
      return x * power(4, N) + y * power(2, N) + z;
    };
    
    const arr = range(power(power(2, N), 3)).map(_ => []);
    for (let j = 0; j < n; j++) {
      const q = pos[j];
      arr[to_index(q)].push({ q, j });
    }

    const uf = new UnionFind(n);
    for (let i = 0; i < n; i++) {
      const p = this.data[i].position;
      for (const { q, j } of arr[to_index(p)]) {
        if (p.sub(q).length() <= eps) uf.unite(i, j);
      }
    }
    
    const se = new Set();
    for (let i = 0; i < n; i++) se.add(uf.root(i));
    const m = se.size;
    const di = new Map();
    {
      let i = 0;
      for (const s of se) {
        di[s] = i++;
      }
    }
    const list = range(m).map(_ => []);
    for (let i = 0; i < n; i++) {
      list[di[uf.root(i)]].push(this.data[i]);
    }
    const data = range(m);
    for (let i = 0; i < m; i++) {
      const l = list[i].length;
      const dat = list[i].reduce((a, c) => {
        return {
          position: a.position.add(c.position),
          color: range(3).map(i => a.color[i] + c.color[i]),
        }
      });
      data[i] = {
        position: dat.position.scale(1 / l),
        color: dat.color.map(d => d / l),
      };
    }
    const t_index = this.index.map(idx => range(3).map(i => di[uf.root(idx[i])]));
    const index = [], tridata = [];
    for (let i = 0; i < t_index.length; i++) {
      if (t_index[i][0] === t_index[i][1] ||
          t_index[i][1] === t_index[i][2] ||
          t_index[i][2] === t_index[i][0]) {
        continue;
      }
      index.push(t_index[i]);
      tridata.push(this.tridata[i]);
    }
    return new Polygon(data, index, tridata);
  }
}

class Model {
  constructor(a, b, c) {
    if (a instanceof WebGLRenderingContext) {
      const gl = a;
      const polygon = b;
      const { position, color, normal, index } = polygon.primitive();

      this.position = buffer(gl, gl.ARRAY_BUFFER, new Float32Array(position));
      this.color    = buffer(gl, gl.ARRAY_BUFFER, new Float32Array(color));
      this.normal   = buffer(gl, gl.ARRAY_BUFFER, new Float32Array(normal));
      this.index    = buffer(gl, gl.ELEMENT_ARRAY_BUFFER, new Int16Array(index));
  
      this.index_length = index.length;
      this.m_matrix = M4();
      this.r_matrix = M4();
    } else if (a instanceof Model) {
      const model = a;
      const m_matrix = b;
      const r_matrix = c;
      this.position = model.position;
      this.color = model.color;
      this.normal = model.normal;
      this.index = model.index;

      this.index_length = model.index_length;
      this.m_matrix = m_matrix;
      this.r_matrix = r_matrix;
    } else {
      throw new Error("Unknown arguments");
    }
  }
  draw(gl, prg, params = {}) {
    this.draw_by_params(gl, prg, params);
  }
  draw_by_params(gl, prg, {
    light = V3(1, 1, 1),
    pos_name = "position",
    col_name = "color",
    nor_name = "normal",
    m_mat_name = "m_matrix",
    r_mat_name = "r_matrix",
    light_name = "light",
  }) {
    const list = [];
    pos_name && list.push([this.position, pos_name, 3]);
    col_name && list.push([this.color,    col_name, 4]);
    nor_name && list.push([this.normal,   nor_name, 3]);

    list.forEach(([vbo, name, stride]) => {
      gl.bindBuffer(gl.ARRAY_BUFFER, vbo);
      const loc = gl.getAttribLocation(prg, name);
      gl.enableVertexAttribArray(loc);
      gl.vertexAttribPointer(loc, stride, gl.FLOAT, false, 0, 0);
    });
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, this.index);

    m_mat_name && uniform(gl, prg, "mat4", m_mat_name, this.m_matrix.primitive());
    r_mat_name && uniform(gl, prg, "mat4", r_mat_name, this.r_matrix.primitive());
    light_name && uniform(gl, prg, "vec3", light_name, light.normalize().primitive());

    gl.drawElements(gl.TRIANGLES, this.index_length, gl.UNSIGNED_SHORT, 0);
  }
  scale(...args) {
    return this.merge(this.m_matrix.scale(...args));
  }
  rotate(...args) {
    return this.merge(this.m_matrix.rotate(...args), this.r_matrix.rotate(...args));
  }
  translate(...args) {
    return this.merge(this.m_matrix.translate(...args));
  }
  lookAt(...args) {
    return this.merge(this.m_matrix.lookAt(...args));
  }
  ortho(...args) {
    return this.merge(this.m_matrix.ortho(...args));
  }
  perspective(...args) {
    return this.merge(this.m_matrix.perspective(...args));
  }
  merge(m_matrix, r_matrix = this.r_matrix) {
    return new Model(this, m_matrix, r_matrix);
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
    parts.rotate(V3(0, 1, 0),  90),
    parts.rotate(V3(0, 1, 0), 180),
    parts.rotate(V3(0, 1, 0), 270),
    parts.rotate(V3(1, 0, 0), 180),
    parts.rotate(V3(1, 0, 0), 180).rotate(V3(0, 1, 0),  90),
    parts.rotate(V3(1, 0, 0), 180).rotate(V3(0, 1, 0), 180),
    parts.rotate(V3(1, 0, 0), 180).rotate(V3(0, 1, 0), 270),
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

export function one_eighth_sphere(r, c = [1, 1, 1], row = 16, col = 16) {
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
