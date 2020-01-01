import { Mat4 } from ".";

export declare function V3(x: number, y: number, z:number): Vec3;

export declare class Vec3 {
  constructor(x: number, y: number, z:number);
  constructor(xyz: number[]);
  constructor(xyz: Float32Array);
  add(that: Vec3): Vec3;
  sub(that: Vec3): Vec3;
  dot(that: Vec3): number;
  cross(that: Vec3): Vec3;
  scale(n: number): Vec3;
  length(): number;
  normalize(): Vec3;
  rotate(axis: Vec3, angle: number): Vec3;
  primitive(): number[];
  equals(that: Vec3): boolean;
  is_zero(): boolean;
  inverse(): Vec3;
  transform(m: Mat4): Vec3;
}
