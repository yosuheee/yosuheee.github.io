import { Vec3 } from "./math/geometry/vec3";
import { Model } from "./model";
import { Lines, Points } from "./primitives";

export declare class Triangles {
  constructor(data: { position: Vec3, color: number[] }[], index: number[][], tridata?: object[]);
  add(that: Triangles): Triangles;
  scale(x: number, y: number, z: number): Triangles;
  rotate(axis: Vec3, angle: number): Triangles;
  translate(x: number, y: number, z: number): Triangles;
  primitive(): { index: number[], position: number[], color: number[] }
  reverse(): Triangles;
  model(gl: WebGLRenderingContext): Model;
  line(): Lines;
  point(): Points;
  flatten(): Triangles;
}
