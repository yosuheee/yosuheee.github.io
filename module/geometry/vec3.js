import { vec3, quat } from "../gl-matrix/index.js";

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
