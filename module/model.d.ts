import { Vec3 } from "./math/geometry/vec3";
import { Triangles } from "./primitives";

type Primitives = Points | Lines | Triangles;

export declare class Model {
  draw(gl: WebGLRenderingContext, prg: WebGLProgram, params?: {
    light?: Vec3,
    pos_name?: string,
    col_name?: string,
    nor_name?: string,
    mvp_mat_name?: string,
    inv_mat_name?: string,
    light_name?: string,
  }): void;
}
