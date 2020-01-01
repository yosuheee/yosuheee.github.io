import { Vec3 } from "./vec3";

export declare function M4(data: number[]): Mat4;
export declare function M4(data: Float32Array): Mat4;

export declare class Mat4 {
  mul(that: Mat4): Mat4;
  invert(): Mat4;
  transpose(): Mat4;
  primitive(): Float32Array;
  scale(x: number, y: number, z: number): Mat4;
  rotate(axis: Vec3, angle: number): Mat4;
  translate(x: number, y: number, z: number): Mat4;
  lookAt(eye: Vec3, center: Vec3, up: Vec3): Mat4;
  ortho(left: number, right: number, bottom: number, top: number, near: number, far: number): Mat4;
  perspective(fovy: number, aspect: number, near: number, far: number): Mat4;
  static scale(x: number, y: number, z: number): Mat4;
  static rotate(axis: Vec3, angle: number): Mat4;
  static translate(x: number, y: number, z: number): Mat4;
  static lookAt(eye: Vec3, center: Vec3, up: Vec3): Mat4;
  static ortho(left: number, right: number, bottom: number, top: number, near: number, far: number): Mat4;
  static perspective(fovy: number, aspect: number, near: number, far: number): Mat4;
}
