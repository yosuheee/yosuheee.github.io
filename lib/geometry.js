import { vec3, vec4, mat4, quat } from "./gl-matrix/index.js";

export function V3(x, y, z) {
  return new Vec3(x, y, z);
}

class Vec3 {
  constructor(x, y, z) {
    [this.x, this.y, this.z] = x.length != null ? x : [x, y, z];
  }
  add(that) {
    const out = vec3.create();
    vec3.add(out, this.primitive(), that.primitive());
    return new Vec3(out);
  }
  sub(that) {
    const out = vec3.create();
    vec3.sub(out, this.primitive(), that.primitive());
    return new Vec3(out);
  }
  dot(that) {
    return vec3.dot(this.primitive(), that.primitive());
  }
  cross(that) {
    const out = vec3.create();
    vec3.cross(out, this.primitive(), that.primitive());
    return new Vec3(out);
  }
  scale(n) {
    const out = vec3.create();
    vec3.scale(out, this.primitive(), n);
    return new Vec3(out);
  }
  length() {
    return vec3.length(this.primitive());
  }
  normalize() {
    const out = vec3.create();
    vec3.normalize(out, this.primitive());
    return new Vec3(out);
  }
  rotate(axis, angle) {
    const q = quat.create();
    quat.setAxisAngle(q, axis.primitive(), Math.PI / 180 * angle);
    const out = vec3.create();
    vec3.transformQuat(out, this.primitive(), q);
    return new Vec3(out);
  }
  primitive() {
    return [this.x, this.y, this.z];
  }
  equals(that) {
    return this.x === that.x && this.y === that.y && this.z === that.z;
  }
  is_zero() {
    return this.equals(V3(0, 0, 0));
  }
  inverse() {
    return this.scale(-1);
  }
  transform(m) {
    const out = vec3.create();
    vec3.transformMat4(out, this.primitive(), m.primitive());
    return new Vec3(out);
  }
}

export function M4(data) {
  return new Mat4(data);
}

export class Mat4 {
  constructor(data = mat4.create()) {
    this.data = data;
  }
  mul(that) {
    const out = mat4.create();
    mat4.mul(out, this.primitive(), that.primitive());
    return new Mat4(out);
  }
  invert() {
    const out = mat4.create();
    mat4.invert(out, this.primitive());
    return new Mat4(out);
  }
  primitive() {
    return this.data;
  }
  scale(...args) {
    return Mat4.scale(...args).mul(this);
  }
  rotate(...args) {
    return Mat4.rotate(...args).mul(this);
  }
  translate(...args) {
    return Mat4.translate(...args).mul(this);
  }
  lookAt(...args) {
    return Mat4.lookAt(...args).mul(this);
  }
  ortho(...args) {
    return Mat4.ortho(...args).mul(this);
  }
  perspective(...args) {
    return Mat4.perspective(...args).mul(this);
  }
  static scale(x, y, z) {
    const out = mat4.create();
    mat4.fromScaling(out, [x, y, z]);
    return new Mat4(out);
  }
  static rotate(axis, angle) {
    const out = mat4.create();
    mat4.fromRotation(out, Math.PI / 180 * angle, axis.primitive());
    return new Mat4(out);
  }
  static translate(x, y, z) {
    const out = mat4.create();
    mat4.fromTranslation(out, [x, y, z]);
    return new Mat4(out);
  }
  static lookAt(eye, center, up) {
    const out = mat4.create();
    mat4.lookAt(out, eye.primitive(), center.primitive(), up.primitive());
    return new Mat4(out);
  }
  static ortho(left, right, bottom, top, near, far) {
    const out = mat4.create();
    mat4.ortho(out, left, right, bottom, top, near, far);
    return new Mat4(out);
  }
  static perspective(fovy, aspect, near, far) {
    const out = mat4.create();
    mat4.perspective(out, fovy, aspect, near, far);
    return new Mat4(out);
  }
}

export function intersection_of_plane_and_line(A, B, C, D, E) {
  const F = E.sub(D);

  const N = B.sub(A).cross(C.sub(A));
  const a = N.x;
  const b = N.y;
  const c = N.z;
  const d = -a * A.x - b * A.y - c * A.z;

  if (N.dot(F) === 0) {
    return V3(NaN, NaN, NaN);
  }
  const t = -(N.dot(D) + d) / N.dot(F);

  const x = D.x + t * F.x;
  const y = D.y + t * F.y;
  const z = D.z + t * F.z;

  return V3(x, y, z);
}

export function triangle_contains_point(A, B, C, Q) {
  const ab = B.sub(A), aq = Q.sub(A);
  const bc = C.sub(B), bq = Q.sub(B);
  const ca = A.sub(C), cq = Q.sub(C);
  const a = ab.cross(aq);
  const b = bc.cross(bq);
  const c = ca.cross(cq);
  if (a.is_zero()) return ab.dot(aq) >= 0 && ab.length() >= aq.length();
  if (b.is_zero()) return bc.dot(bq) >= 0 && bc.length() >= cq.length();
  if (c.is_zero()) return ca.dot(cq) >= 0 && ca.length() >= cq.length();
  return a.dot(b) >= 0 && b.dot(c) >= 0 && c.dot(a);
}

export function grid_rect_collision_lt(x1, y1, x2, y2, x3, y3, x4, y4) {
  console.assert(x1 <= x2 && y1 <= y2 && x3 <= x4 && y3 <= y4);
  const c1 = { x: x1 + (x2 - x1) / 2,
               y: y1 + (y2 - y1) / 2 };
  const c2 = { x: x3 + (x4 - x3) / 2,
               y: y3 + (y4 - y3) / 2 };
  return Math.abs(c1.x - c2.x) * 2 < ((x2 - x1) + (x4 - x3))
      && Math.abs(c1.y - c2.y) * 2 < ((y2 - y1) + (y4 - y3));
}
