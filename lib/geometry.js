import { vec3, quat } from "./gl-matrix/index.js";

export function Vec3(x, y, z) {
  return new Vector3(x, y, z);
}

class Vector3 {
  constructor(x, y, z) {
    if (x.length != null) {
      [this.x, this.y, this.z] = x;
    } else {
      [this.x, this.y, this.z] = [x, y, z];
    }
  }
  add(that) {
    const out = vec3.create();
    vec3.add(out, this.primitive(), that.primitive());
    return new Vector3(out);
  }
  sub(that) {
    const out = vec3.create();
    vec3.sub(out, this.primitive(), that.primitive());
    return new Vector3(out);
  }
  dot(that) {
    return vec3.dot(this.primitive(), that.primitive());
  }
  cross(that) {
    const out = vec3.create();
    vec3.cross(out, this.primitive(), that.primitive());
    return new Vector3(out);
  }
  scale(n) {
    const out = vec3.create();
    vec3.scale(out, this.primitive(), n);
    return new Vector3(out);
  }
  length() {
    return vec3.length(this.primitive());
  }
  normalize() {
    const out = vec3.create();
    vec3.normalize(out, this.primitive());
    return new Vector3(out);
  }
  rotate(axis, angle) {
    const q = quat.create();
    quat.setAxisAngle(q, axis.primitive(), Math.PI / 180 * angle);
    const out = vec3.create();
    vec3.transformQuat(out, this.primitive(), q);
    return new Vector3(out);
  }
  primitive() {
    return [this.x, this.y, this.z];
  }
  equals(that) {
    return this.x === that.x && this.y === that.y && this.z === that.z;
  }
  is_zero() {
    return this.equals(Vec3(0, 0, 0));
  }
  inverse() {
    return this.scale(-1);
  }
}

export function intersection_of_plane_and_line(A, B, C, D, E) {
  const F = E.sub(D);

  const N = B.sub(A).cross(C.sub(A));
  const a = N.x;
  const b = N.y;
  const c = N.z;
  const d = - a * A.x - b * A.y - c * A.z;

  const t = - (a * D.x + b * D.y + c * D.z + d) / (a * F.x + b * F.y + c * F.z);

  const x = D.x + t * F.x;
  const y = D.y + t * F.y;
  const z = D.z + t * F.z;

  return Vec3(x, y, z);
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
  return a.dot(b) >= 0 && a.dot(c) >= 0;
}
