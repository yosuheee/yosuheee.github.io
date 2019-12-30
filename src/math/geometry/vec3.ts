import { vec3, quat } from "gl-matrix";
import { Mat4 } from "./mat4";

export function V3(x: number, y: number, z: number) {
  return new Vec3(x, y, z);
}

export class Vec3 {
  x: number;
  y: number;
  z: number;
  constructor(x: any, y?: number, z?: number) {
    [this.x, this.y, this.z] = x.length != null ? x : [x, y, z];
  }
  add(that: Vec3) {
    const out = vec3.create();
    vec3.add(out, this.primitive(), that.primitive());
    return new Vec3(out);
  }
  sub(that: Vec3) {
    const out = vec3.create();
    vec3.sub(out, this.primitive(), that.primitive());
    return new Vec3(out);
  }
  dot(that: Vec3) {
    return vec3.dot(this.primitive(), that.primitive());
  }
  cross(that: Vec3) {
    const out = vec3.create();
    vec3.cross(out, this.primitive(), that.primitive());
    return new Vec3(out);
  }
  scale(n: number) {
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
  rotate(axis: Vec3, angle: number) {
    const q = quat.create();
    quat.setAxisAngle(q, axis.primitive(), Math.PI / 180 * angle);
    const out = vec3.create();
    vec3.transformQuat(out, this.primitive(), q);
    return new Vec3(out);
  }
  primitive() {
    return [this.x, this.y, this.z];
  }
  equals(that: Vec3) {
    return this.x === that.x && this.y === that.y && this.z === that.z;
  }
  is_zero() {
    return this.equals(V3(0, 0, 0));
  }
  inverse() {
    return this.scale(-1);
  }
  transform(m: Mat4) {
    const out = vec3.create();
    vec3.transformMat4(out, this.primitive(), m.primitive());
    return new Vec3(out);
  }
}
