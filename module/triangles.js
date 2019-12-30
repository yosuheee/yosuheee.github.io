import { V3, x_axis, y_axis, z_axis } from "./math/geometry/index.js";
import { Triangles } from "./primitives.js";

export { Triangles };

export function rect(x, y, color = [1, 1, 1]) {
  const data = [
    { position: V3(0, 0, 0), color },
    { position: V3(x, 0, 0), color },
    { position: V3(0, y, 0), color },
    { position: V3(x, y, 0), color },
  ];
  return new Triangles(data, [[0, 1, 2], [2, 1, 3]]);
}

export function quarter_cylinder_rect(h, r, color = [1, 1, 1], c = 32) {
  const data = [], index = [];
  for (let i = 0; i <= c; i++) {
    const v = V3(0, r, 0).rotate(x_axis, 90 / c * i);
    data.push({ position: v, color });
    data.push({ position: v.add(V3(h, 0, 0)), color });
  }
  for (let i = 0; i < c; i++) {
    const j = i * 2;
    index.push([j, j + 2, j + 1]);
    index.push([j + 2, j + 3, j + 1]);
  }
  return new Triangles(data, index);
}

export function one_eighth_sphere(r, color = [1, 1, 1], c = 16) {
  const data = [], index = [];
  for (let i = 0; i <= c; i++) {
    const v = V3(r, 0, 0).rotate(z_axis, 90 / c * i);
    for (let j = 0; j <= c; j++) {
      const p = v.rotate(y_axis, -90 / c * j);
      data.push({ position: p, color });
    }
  }
  for (let i = 0; i < c; i++)
  for (let j = 0; j < c; j++) {
    const s = (i + 0) * (c + 1) + j;
    const t = (i + 1) * (c + 1) + j;
    index.push([s, t, s + 1]);
    index.push([s + 1, t, t + 1]);
  }
  return new Triangles(data, index);
}

export function torus(lr = 0.55, sr = 0.25, color = [1, 1, 1], lc = 64, sc = 64) {
  const data = [], index = [];
  for (let i = 0; i <= lc; i++) {
    const o = V3(lr, 0, 0).rotate(y_axis, 360 / lc * i);
    const r = o.normalize().scale(sr);
    const n = o.normalize().cross(y_axis);
    for (let j = 0; j <= sc; j++) {
      const p = r.rotate(n, 360 / sc * j).add(o);
      data.push({ position: p, color });
    }
  }
  for (let i = 0; i < lc; i++)
  for (let j = 0; j < sc; j++) {
    const s = (i + 0) * (sc + 1) + j;
    const t = (i + 1) * (sc + 1) + j;
    index.push([s, t, s + 1]);
    index.push([s + 1, t, t + 1]);
  }
  return new Triangles(data, index).flatten();
}

export function sphere(r, color = [1, 1, 1], c = 16) {
  const parts = one_eighth_sphere(r, color, c);
  const objects = [
    parts,
    parts.rotate(y_axis,  90),
    parts.rotate(y_axis, 180),
    parts.rotate(y_axis, 270),
    parts.rotate(x_axis, 180),
    parts.rotate(x_axis, 180).rotate(y_axis,  90),
    parts.rotate(x_axis, 180).rotate(y_axis, 180),
    parts.rotate(x_axis, 180).rotate(y_axis, 270),
  ];
  return objects.reduce((a, c) => a.add(c));
}

export function cube(w, color = [1, 1, 1]) {
  const r = rect(w, w, color);
  const rects = [
    r,
    r.rotate(x_axis, -90).translate(0, w, 0),
    r.rotate(x_axis,  90).translate(0, 0, -w),
    r.rotate(y_axis,  90).translate(w, 0, 0),
    r.rotate(y_axis, -90).translate(0, 0, -w),
    r.rotate(y_axis, 180).translate(w, 0, -w),
  ];
  return rects.reduce((a, c) => a.add(c));
}

export function rounded_corners_cube(w, r, color = [1, 1, 1]) {
  const face = rect(w, w, color);
  const border = quarter_cylinder_rect(w, r, color);
  const corner = one_eighth_sphere(r, color);

  const border4 = [
    border,
    border.rotate(x_axis,  90).translate(0, -w,  0),
    border.rotate(x_axis, 180).translate(0, -w, -w),
    border.rotate(x_axis, -90).translate(0,  0, -w),
  ].reduce((a, c) => a.add(c));

  const corner4 = [
    corner,
    corner.rotate(x_axis,  90).translate(0, -w,  0),
    corner.rotate(x_axis, 180).translate(0, -w, -w),
    corner.rotate(x_axis, -90).translate(0,  0, -w),
  ].reduce((a, c) => a.add(c));

  const objects = [
    face.translate(r, r, 0),
    face.rotate(x_axis, -90).translate(0, w,  0).translate(r, r * 2, -r),
    face.rotate(x_axis,  90).translate(0, 0, -w).translate(r, 0, -r),
    face.rotate(y_axis,  90).translate(w, 0,  0).translate(r * 2, r, -r),
    face.rotate(y_axis, -90).translate(0, 0, -w).translate(0, r, -r),
    face.rotate(y_axis, 180).translate(w, 0, -w).translate(r, r, -r * 2),
    border4.translate(r, w + r, -r),
    border4.rotate(y_axis, 90).translate(r + w, r + w, -r),
    border4.rotate(z_axis, 90).translate(r, r, -r),
    corner4.translate(w + r, w + r, -r),
    corner4.rotate(z_axis, 180).translate(r, r, -r),
  ];
  return objects.reduce((a, c) => a.add(c));
}
