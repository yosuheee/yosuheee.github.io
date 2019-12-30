import { mat4 } from "./gl-matrix/index.js";

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
  transpose() {
    const out = mat4.create();
    mat4.transpose(out, this.primitive());
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
