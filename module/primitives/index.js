import { V3, Mat4, M4 } from "../geometry.js";
import { range } from "../util.js";
import { power } from "../math.js";
import { uniform, buffer, attribute } from "../webgl.js";
import { UnionFind } from "../union-find.js";

export class Points {
  constructor(data = [], index = []) {
    this.data = data;
    this.index = index;
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
    return new Points(this.data.map(d => {
      return { ...d, position: d.position.transform(m)};
    }), this.index);
  }
  primitive() {
    const index = this.index.flat();
    const position = this.data.map(d => d.position.primitive()).flat();
    const color = this.data.map(d => d.color.length === 3 ? d.color.concat([1.0]) : d.color).flat();
    return { index, position, color };
  }
  model(gl) {
    return new Model({ gl, primitives: this, type: gl.POINTS });
  }
}

export class Lines {
  constructor(data = [], index = []) {
    this.data = data;
    this.index = index;
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
    return new Lines(this.data.map(d => {
      return { ...d, position: d.position.transform(m)};
    }), this.index);
  }
  primitive() {
    const index = this.index.flat();
    const position = this.data.map(d => d.position.primitive()).flat();
    const color = this.data.map(d => d.color.length === 3 ? d.color.concat([1.0]) : d.color).flat();
    return { index, position, color };
  }
  model(gl) {
    return new Model({ gl, primitives: this, type: gl.LINES });
  }
}

export class Triangles {
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
    return new Triangles(data, index, tridata);
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
    return new Triangles(this.data.map(d => {
      return { ...d, position: d.position.transform(m)};
    }), this.index, this.tridata);
  }
  reverse() {
    return new Triangles(this.data, this.index.map(i => [i[0], i[2], i[1]]), this.tridata);
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
    return new Model({ gl, primitives: this, type: gl.TRIANGLES });
  }
  line() {
    const index = [];
    for (const indices of this.index) {
      const a = indices[0];
      const b = indices[1];
      const c = indices[2];
      index.push([a, b], [b, c], [c, a]);
    }
    return new Lines(this.data, index);
  }
  point() {
    return new Points(this.data, range(this.data.length));
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
    const to_index = (() => {
      const min = V3(
        pos.reduce((a, c) => Math.min(a, c.x), 1e9),
        pos.reduce((a, c) => Math.min(a, c.y), 1e9),
        pos.reduce((a, c) => Math.min(a, c.z), 1e9),
      );
      const max = V3(
        pos.reduce((a, c) => Math.max(a, c.x), -1e9) + 0.01,
        pos.reduce((a, c) => Math.max(a, c.y), -1e9) + 0.01,
        pos.reduce((a, c) => Math.max(a, c.z), -1e9) + 0.01,
      );
      return p => {
        const x = Math.floor(p.sub(min).x / max.sub(min).x * power(2, N));
        const y = Math.floor(p.sub(min).y / max.sub(min).y * power(2, N));
        const z = Math.floor(p.sub(min).z / max.sub(min).z * power(2, N));
        return x * power(4, N) + y * power(2, N) + z;
      };
    })();
    
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
    return new Triangles(data, index, tridata);
  }
}

class Model {
  constructor({
    primitives = null,
    model      = null,
    gl         = null,
    type       = null,
    mvp_matrix = M4(),
    m_matrix   = M4(),
  }) {
    if (type == null) {
      throw new Error("must exist type");
    }
    if ((primitives == null || gl == null) && model == null) {
      throw new Error("must exist primitives or model");
    }
    if ((primitives != null && gl != null) && model != null) {
      throw new Error("must not be both primitives and model");
    }
    if (primitives != null && gl != null) {
      const { position, color, index, normal } = primitives.primitive();

      this.position = buffer(gl, gl.ARRAY_BUFFER, new Float32Array(position));
      if (color  != null) this.color  = buffer(gl, gl.ARRAY_BUFFER, new Float32Array(color));
      if (normal != null) this.normal = buffer(gl, gl.ARRAY_BUFFER, new Float32Array(normal));

      this.index = buffer(gl, gl.ELEMENT_ARRAY_BUFFER, new Int16Array(index));
      this.length = index.length;
    }
    if (model != null) {
      this.position = model.position;
      this.color    = model.color;
      this.normal   = model.normal;
      this.index    = model.index;
      this.length   = model.length;
    }
    this.type       = type;
    this.mvp_matrix = mvp_matrix;
    this.m_matrix   = m_matrix;
  }
  draw(gl, prg, params = {}) {
    this.draw_by_params(gl, prg, params);
  }
  draw_by_params(gl, prg, {
    light = V3(1, 1, 1),
    pos_name = "position",
    col_name = "color",
    nor_name = "normal",
    mvp_mat_name = "mvp_matrix",
    inv_mat_name = "inv_matrix",
    light_name = "light",
  }) {
    const list = [];
    list.push([this.position, pos_name, 3]);
    if (this.color  != null && col_name) list.push([this.color,  col_name, 4]);
    if (this.normal != null && nor_name) list.push([this.normal, nor_name, 3]);

    list.forEach(([vbo, name, stride]) => attribute(gl, prg, vbo, name, stride));
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, this.index);

    mvp_mat_name && uniform(gl, prg, "mat4", mvp_mat_name, this.mvp_matrix.primitive());
    inv_mat_name && uniform(gl, prg, "mat4", inv_mat_name, this.m_matrix.invert().transpose().primitive());
    light_name && uniform(gl, prg, "vec3", light_name, light.normalize().primitive());

    gl.drawElements(this.type, this.length, gl.UNSIGNED_SHORT, 0);
  }
  scale(...args) {
    return this.merge(this.mvp_matrix.scale(...args), this.m_matrix.scale(...args));
  }
  rotate(...args) {
    return this.merge(this.mvp_matrix.rotate(...args), this.m_matrix.rotate(...args));
  }
  translate(...args) {
    return this.merge(this.mvp_matrix.translate(...args), this.m_matrix.translate(...args));
  }
  lookAt(...args) {
    return this.merge(this.mvp_matrix.lookAt(...args));
  }
  ortho(...args) {
    return this.merge(this.mvp_matrix.ortho(...args));
  }
  perspective(...args) {
    return this.merge(this.mvp_matrix.perspective(...args));
  }
  merge(mvp_matrix, m_matrix = this.m_matrix) {
    return new Model({ model: this, type: this.type, mvp_matrix, m_matrix });
  }
}
