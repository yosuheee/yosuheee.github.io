import { V3 } from "/lib/geometry.js";
import { Polygon, one_eighth_sphere, quarter_cylinder_rect, rect } from "../lib/polygon.js";

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
    data.push({ position: V3(rx + radius, ry + radius, 0), color: c });
    data.push({ position: V3(x + radius, y + radius, 0), color: c });
  }
  for (let i = 0; i < count; i++) {
    const std = i * 2;
    index.push([std, std + 1, std + 2]);
    index.push([std + 1, std + 3, std + 2]);
  }
  return new Polygon(data, index);
}

export function reverse_half_sphere(r, c = [1, 1, 1], row = 16, col = 16) {
  const oes = one_eighth_sphere(r, c, row, col);
  const parts = [
    oes,
    oes.rotate(V3(0, 0, 1),  90),
    oes.rotate(V3(0, 0, 1), 180),
    oes.rotate(V3(0, 0, 1), 270),
  ];
  return parts.reduce((a, c) => a.add(c))
              .rotate(V3(1, 0, 0), 180)
              .translate(r, r, 0)
              .reverse();
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
        .rotate(V3(0, 1, 0), 90)
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
        .rotate(V3(1, 0, 0), -90)
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
        .rotate(V3(1, 0, 0), 90)
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
        .rotate(V3(0, 1, 0), -90)
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
        .rotate(V3(1, 0, 0), 180)
        .translate(R, E + R, -E - 2 * R);
    })(),
  ];
  
  const kama = quarter_cylinder_rect(E, R).rotate(V3(0, 1, 0), 90);
  const borders = [
    kama.translate(E + R, E + R, -R),
    kama.rotate(V3(1, 0, 0), -90).translate(E + R, E + R, -E - R),
    kama.rotate(V3(1, 0, 0),  90).translate(E + R, R, -R),
    kama.rotate(V3(0, 1, 0), -90).translate(R, E + R, -R),
    kama.rotate(V3(0, 1, 0),  90).translate(E + R, E + R, -E - R),
    kama.rotate(V3(0, 1, 0), 180).translate(R, E + R, -E - R),
    kama.rotate(V3(0, 0, 1), -90).translate(E + R, R, -R),
    kama.rotate(V3(0, 0, 1), 180).translate(R, R, -R),
    kama.rotate(V3(1, 0, 0), -90).rotate(V3(0, 1, 0),  90).translate(R, E + R, -E - R),
    kama.rotate(V3(1, 0, 0),  90).rotate(V3(0, 1, 0), -90).translate(R, R, -R),
    kama.rotate(V3(0, 1, 0), -90).rotate(V3(1, 0, 0),  90).translate(R, R, -R),
    kama.rotate(V3(0, 1, 0), -90).rotate(V3(1, 0, 0), 180).translate(R, R, -E - R),
  ];
  const kado = one_eighth_sphere(R, [1, 1, 1], 16, 16);
  const kado4 = [
    kado.translate(E + R, E + R, -R),
    kado.rotate(V3(0, 1, 0), -90).translate(R, E + R, -R),
    kado.rotate(V3(0, 0, 1), -90).translate(E + R, R, -R),
    kado.rotate(V3(0, 0, 1), 180).translate(R, R, -R),
  ].reduce((a, c) => a.add(c));
  const corners = [
    kado4,
    kado4.rotate(V3(1, 0, 0), 180).translate(0, E + R * 2, -E - R * 2),
  ];

  return [ ...objects, ...borders, ...corners ].reduce((a, c) => a.add(c));
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
        objects.push({ o: ana(w / 2).add(reverse_half_sphere(w / 2, C)), x, y });
      } else {
        objects.push({ o: rect(w, h), x, y });
      }
    }
  }
  
  return objects.map(r => r.o.translate(r.x, r.y, 0)).reduce((a, c) => a.add(c));
}
