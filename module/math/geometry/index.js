import { V3 } from "./vec3.js";
import { M4, Mat4 } from "./mat4.js";

export { V3, M4, Mat4 };

export const x_axis = V3(1, 0, 0);
export const y_axis = V3(0, 1, 0);
export const z_axis = V3(0, 0, 1);

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
