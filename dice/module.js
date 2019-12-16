import { vec3, mat4 } from "/lib/gl-matrix/index.js";
import { Vec3 } from "/lib/geometry.js";

class Polygon {
  constructor(data = [], index = []) {
    this.data = data;
    this.index = index;
  }
  add(poly) {
    const index = this.index.concat(
      poly.index.map(i => i + this.data.length)
    );
    const data = this.data.concat(poly.data);
    return new Polygon(data, index);
  }
  rotate(axis, angle) {
    return new Polygon(this.data.map(d => {
      const m = mat4.create();
      const p = vec3.create();
      mat4.fromRotation(m, Math.PI / 180 * angle, axis);
      vec3.transformMat4(p, d.position, m);
      return { ...d, position: Array.from(p) };
    }), this.index);
  }
  translate(x, y, z) {
    return new Polygon(this.data.map(d => {
      const m = mat4.create();
      const p = vec3.create();
      mat4.fromTranslation(m, [x, y, z]);
      vec3.transformMat4(p, d.position, m);
      return { ...d, position: Array.from(p) };
    }), this.index);
  }
  primitive() {
    const index = this.index;
    const position = this.data.map(d => d.position).flat();
    const color = this.data.map(d => d.color).flat();

    const normal = this.data.map(_ => Vec3(0, 0, 0));
    for (let i = 0; i < index.length; i += 3) {
      const a = Vec3(position.slice(index[i + 0] * 3, index[i + 0] * 3 + 3));
      const b = Vec3(position.slice(index[i + 1] * 3, index[i + 1] * 3 + 3));
      const c = Vec3(position.slice(index[i + 2] * 3, index[i + 2] * 3 + 3));
      const n = b.sub(a).cross(c.sub(a)).normalize();

      normal[index[i + 0]] = normal[index[i + 0]].add(n).normalize();
      normal[index[i + 1]] = normal[index[i + 1]].add(n).normalize();
      normal[index[i + 2]] = normal[index[i + 2]].add(n).normalize();
    }
    return { index, position, color, normal: normal.map(n => n.primitive()).flat() };
  }
  model(gl) {
    return new Model(gl, this);
  }
}

class Model {
  constructor(gl, polygon) {
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

    this.index = index;
  }
  prepare(gl, prg) {
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

    const index = gl.createBuffer();
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, index);
    gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, new Int16Array(this.index), gl.STATIC_DRAW);
  }
}

export function ana(radius, count = 32, c = [1, 1, 1]) {
  const data = [], index = [];
  for (let i = 0; i <= count; i++) {
    const angle = Math.PI * 2 / count * i;
    const rx = radius * Math.cos(angle);
    const ry = radius * Math.sin(angle);
    const j = Math.floor(i / (count / 8)) % 8;
    let x, y;
    if (j == 0 || j == 7) x = radius, y = ry / Math.cos(angle);
    if (j == 1 || j == 2) x = rx / Math.sin(angle), y = radius;
    if (j == 3 || j == 4) x = -radius, y = -ry / Math.cos(angle);
    if (j == 5 || j == 6) x = -rx / Math.sin(angle), y = -radius;
    data.push({ position: [rx + radius, ry + radius, 0], color: c, normal: [0, 0, 1] });
    data.push({ position: [x + radius, y + radius, 0], color: c, normal: [0, 0, 1] });
  }
  for (let i = 0; i < count; i++) {
    const std = i * 2;
    index.push(std, std + 1, std + 2);
    index.push(std + 1, std + 3, std + 2);
  }
  return new Polygon(data, index);
}

export function naka(radius, c = [1, 1, 1], large = 32, small = 32) {
  const data = [], index = [];
  for (let i = 0; i <= large; i++) {
    const langle = Math.PI / 2 / large * i;
    const rad = radius * Math.cos(langle);
    const z = -radius * Math.sin(langle);
    for (let j = 0; j <= small; j++) {
      const sangle = Math.PI * 2 / small * j;
      const x = rad * Math.cos(sangle);
      const y = rad * Math.sin(sangle);
      const normal = [
        Math.cos(langle) * -Math.cos(sangle),
        Math.cos(langle) * -Math.sin(sangle),
        Math.sin(langle),
      ];
      data.push({ position: [x + radius, y + radius, z], color: c, normal });
    }
  }
  for (let i = 0; i < large; i++) {
    for (let j = 0; j < small; j++) {
      const std1 = i * (small + 1) + j;
      const std2 = (i + 1) * (small + 1) + j;
      index.push(std1, std1 + 1, std2);
      index.push(std1 + 1, std2 + 1, std2);
    }
  }
  return new Polygon(data, index);
}

export function rect(x, y, c = [1, 1, 1]) {
  const data = [], index = [];
  data.push({ position: [0, 0, 0], color: c, normal: [0, 0, 1] });
  data.push({ position: [x, 0, 0], color: c, normal: [0, 0, 1] });
  data.push({ position: [0, y, 0], color: c, normal: [0, 0, 1] });
  data.push({ position: [x, y, 0], color: c, normal: [0, 0, 1] });
  index.push(0, 1, 2);
  index.push(2, 1, 3);
  return new Polygon(data, index);
}

export function kamaboko(radius, length, c = [1, 1, 1], count = 32) {
  const data = [], index = [];
  for (let i = 0; i <= count; i++) {
    const angle = Math.PI / 180 * 90 / count * i;
    const x = radius * Math.cos(angle);
    const y = radius * Math.sin(angle);
    data.push({ position: [x, y, 0], color: c, normal: [x, y, 0] });
    data.push({ position: [x, y, -length], color: c, normal: [x, y, 0] });
  }
  for (let i = 0; i < count; i++) {
    const j = i * 2;
    index.push(j, j + 1, j + 2);
    index.push(j + 2, j + 1, j + 3);
  }
  return new Polygon(data, index);
}

export function eight(radius, row, col, c = [1, 1, 1]) {
  const data = [], index = [];
  for (let i = 0; i <= row; i++) {
    const angle = Math.PI / 180 * 90 / row * i;
    const r = radius * Math.cos(angle);
    const z = radius * Math.sin(angle);
    for (let j = 0; j <= col; j++) {
      const a = Math.PI / 180 * 90 / col * j;
      const x = r * Math.cos(a);
      const y = r * Math.sin(a);
      const normal = [
        Math.cos(angle) * Math.cos(a),
        Math.cos(angle) * Math.sin(a),
        Math.sin(angle),
      ];
      data.push({ position: [x, y, z], color: c, normal });
    }
  }
  for (let i = 0; i < row; i++) {
    for (let j = 0; j < col; j++) {
      const s = i * (col + 1) + j;
      const t = (i + 1) * (col + 1) + j;
      index.push(t, s, t + 1);
      index.push(t + 1, s, s + 1);
    }
  }
  return new Polygon(data, index);
}

export function dice(edge = 8, radius = 2) {
  const E = edge;
  const R = radius;
  const Black = [0.2, 0.2, 0.2];
  
  const objects = [
    (() => {
      const M = 5;
      const C = [1, 0, 0];
  
      const S = (E - M) / 2;
      return face([
        { x: S, y: S },
      ], E, M, C)
        .translate(R, R, 0);
    })(),
    (() => {
      const M = 2;
      const D = 3;
      const C = Black;

      const S = (E - 2 * M - D) / 2;
      return face([
        { x: S,         y: S },
        { x: S + M + D, y: S + M + D },
      ], E, M, C)
        .rotate([0, 1, 0], 90)
        .translate(E + 2 * R, R, -R);
    })(),
    (() => {
      const M = 2;
      const D = 1;
      const C = Black;

      const S = (E - M * 3 - D * 2) / 2;
      return face([
        { x: S,                 y: S + M + D + M + D },
        { x: S + M + D,         y: S + M + D },
        { x: S + M + D + M + D, y: S },
      ], E, M, C)
        .rotate([1, 0, 0], -90)
        .translate(R, E + 2 * R, -R);
    })(),
    (() => {
      const M = 2;
      const D = 3;
      const C = Black;

      const S = (E - 2 * M - D) / 2;
      return face([
        { x: S,         y: S },
        { x: S,         y: S + M + D },
        { x: S + M + D, y: S },
        { x: S + M + D, y: S + M + D },
      ], E, M, C)
        .rotate([1, 0, 0], 90)
        .translate(R, 0, -E - R);
    })(),
    (() => {
      const M = 2;
      const D = 1;
      const C = Black;

      const S = (E - M * 3 - D * 2) / 2;
      return face([
        { x: S,                 y: S + M + D + M + D },
        { x: S,                 y: S },
        { x: S + M + D,         y: S + M + D },
        { x: S + M + D + M + D, y: S },
        { x: S + M + D + M + D, y: S + M + D + M + D },
      ], E, M, C)
        .rotate([0, 1, 0], -90)
        .translate(0, R, -E - R);
    })(),
    (() => {
      const M = 2;
      const X = 1;
      const Y = 4;
      const C = Black;

      const S = (E - M * 3 - X * 2) / 2;
      const T = (E - M * 2 - Y) / 2;
      return face([
        { x: S,                 y: T },
        { x: S,                 y: T + M + Y },
        { x: S + M + X,         y: T },
        { x: S + M + X,         y: T + M + Y },
        { x: S + M + X + M + X, y: T },
        { x: S + M + X + M + X, y: T + M + Y },
      ], E, M, C)
        .rotate([1, 0, 0], 180)
        .translate(R, E + R, -E - 2 * R);
    })(),
  ];
  
  const kama = kamaboko(R, E);
  const borders = [
    kama.translate(E + R, E + R, -R),
    kama.rotate([1, 0, 0], -90).translate(E + R, E + R, -E - R),
    kama.rotate([1, 0, 0],  90).translate(E + R, R, -R),
    kama.rotate([0, 1, 0], -90).translate(R, E + R, -R),
    kama.rotate([0, 1, 0],  90).translate(E + R, E + R, -E - R),
    kama.rotate([0, 1, 0], 180).translate(R, E + R, -E - R),
    kama.rotate([0, 0, 1], -90).translate(E + R, R, -R),
    kama.rotate([0, 0, 1], 180).translate(R, R, -R),
    kama.rotate([1, 0, 0], -90).rotate([0, 1, 0],  90).translate(R, E + R, -E - R),
    kama.rotate([1, 0, 0],  90).rotate([0, 1, 0], -90).translate(R, R, -R),
    kama.rotate([0, 1, 0], -90).rotate([1, 0, 0],  90).translate(R, R, -R),
    kama.rotate([0, 1, 0], -90).rotate([1, 0, 0], 180).translate(R, R, -E - R),
  ];
  const kado = eight(R, 16, 16);
  const kado4 = [
    kado.translate(E + R, E + R, -R),
    kado.rotate([0, 1, 0], -90).translate(R, E + R, -R),
    kado.rotate([0, 0, 1], -90).translate(E + R, R, -R),
    kado.rotate([0, 0, 1], 180).translate(R, R, -R),
  ].reduce((a, c) => a.add(c));
  const corners = [
    kado4,
    kado4.rotate([1, 0, 0], 180).translate(0, E + R * 2, -E - R * 2),
  ];

  return [ objects, borders, corners ].flat().reduce((a, c) => a.add(c));
}

function face(points, edge = 10, size = 1, color = [0, 0, 0]) {
  const E = edge;
  const M = size;
  const C = color;

  const xs = [];
  const ys = [];
  {
    const xa = points.map(p => p.x);
    const ya = points.map(p => p.y);
    for (let i = 0, l = xa.length; i < l; i++) {
      xa.push(xa[i] + M);
    }
    for (let i = 0, l = ya.length; i < l; i++) {
      ya.push(ya[i] + M);
    }
    xa.push(0, E);
    ya.push(0, E);
  
    const xset = xa.reduce((a, c) => a.add(c), new Set());
    const yset = ya.reduce((a, c) => a.add(c), new Set());
    for (const v of xset) xs.push(v);
    for (const v of yset) ys.push(v);
    
    const compare = (a, b) => a < b ? -1 : (a > b ? 1 : 0);
    xs.sort(compare);
    ys.sort(compare);
  }

  const objects = [];
  for (let i = 0; i < xs.length - 1; i++) {
    for (let j = 0; j < ys.length - 1; j++) {
      const x = xs[i];
      const y = ys[j];
      const w = xs[i + 1] - xs[i];
      const h = ys[j + 1] - ys[j];
      if (points.some(p => p.x === x && p.y === y)) {
        objects.push({ o: ana(w / 2).add(naka(w / 2, C)), x, y });
      } else {
        objects.push({ o: rect(w, h), x, y });
      }
    }
  }
  
  return objects.map(r => r.o.translate(r.x, r.y, 0)).reduce((a, c) => a.add(c));
}

export function program(gl, vss, fss) {
  const prg = gl.createProgram();
  {
    const vs = gl.createShader(gl.VERTEX_SHADER);
    const fs = gl.createShader(gl.FRAGMENT_SHADER);
    gl.shaderSource(vs, vss);
    gl.shaderSource(fs, fss);
    gl.compileShader(vs);
    gl.compileShader(fs);
    gl.attachShader(prg, vs);
    gl.attachShader(prg, fs);
  }
  gl.linkProgram(prg);
  gl.useProgram(prg);
  return prg;
}

export function uniformMatrix4(gl, prg, name, value) {
  const ul = gl.getUniformLocation(prg, name);
  gl.uniformMatrix4fv(ul, false, value);
}

export function uniform3(gl, prg, name, value) {
  const ul = gl.getUniformLocation(prg, name);
  gl.uniform3fv(ul, value);
}
