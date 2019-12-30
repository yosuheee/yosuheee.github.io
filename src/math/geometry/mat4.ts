import { mat4 } from "gl-matrix";
import { Vec3 } from "./vec3";

export function M4(data = mat4.create()) {
  return new Mat4(data);
}

export class Mat4 {
  data: mat4;
  constructor(data = mat4.create()) {
    this.data = data;
  }
  mul(that: Mat4) {
    const out = mat4.create();
    mat4.mul(out, this.primitive(), that.primitive());
    return new Mat4(out);
  }
  invert() {
    const out = mat4.create();
    mat4.invert(out, this.primitive());
    return new Mat4(out);
  }
  transpose() {
    const out = mat4.create();
    mat4.transpose(out, this.primitive());
    return new Mat4(out);
  }
  primitive() {
    return this.data;
  }
  scale(x: number, y: number, z: number) {
    return Mat4.scale(x, y, z).mul(this);
  }
  rotate(axis: Vec3, angle: number) {
    return Mat4.rotate(axis, angle).mul(this);
  }
  translate(x: number, y: number, z: number) {
    return Mat4.translate(x, y, z).mul(this);
  }
  lookAt(eye: Vec3, center: Vec3, up: Vec3) {
    return Mat4.lookAt(eye, center, up).mul(this);
  }
  ortho(left: number, right: number, bottom: number, top: number, near: number, far: number) {
    return Mat4.ortho(left, right, bottom, top, near, far).mul(this);
  }
  perspective(fovy: number, aspect: number, near: number, far: number) {
    return Mat4.perspective(fovy, aspect, near, far).mul(this);
  }
  static scale(x: number, y: number, z: number) {
    const out = mat4.create();
    mat4.fromScaling(out, [x, y, z]);
    return new Mat4(out);
  }
  static rotate(axis: Vec3, angle: number) {
    const out = mat4.create();
    mat4.fromRotation(out, Math.PI / 180 * angle, axis.primitive());
    return new Mat4(out);
  }
  static translate(x: number, y: number, z: number) {
    const out = mat4.create();
    mat4.fromTranslation(out, [x, y, z]);
    return new Mat4(out);
  }
  static lookAt(eye: Vec3, center: Vec3, up: Vec3) {
    const out = mat4.create();
    mat4.lookAt(out, eye.primitive(), center.primitive(), up.primitive());
    return new Mat4(out);
  }
  static ortho(left: number, right: number, bottom: number, top: number, near: number, far: number) {
    const out = mat4.create();
    mat4.ortho(out, left, right, bottom, top, near, far);
    return new Mat4(out);
  }
  static perspective(fovy: number, aspect: number, near: number, far: number) {
    const out = mat4.create();
    mat4.perspective(out, fovy, aspect, near, far);
    return new Mat4(out);
  }
}
