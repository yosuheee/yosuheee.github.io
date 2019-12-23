import { Vec3,
         intersection_of_plane_and_line,
         triangle_contains_point } from "../lib/geometry.js";

export function* make_positions(v0, h0, stage, qt, {
  r = 0.04267,            // ボールの半径
  m = 0.04593,            // ボールの質量
  W = 0,                  // 風の強さ
  D = Math.PI / 180 * 0,  // 風の角度
  k = 0.00015,            // 空気抵抗係数
}) {
  const g = Vec3(0, -9.8 / 3600, 0);
  const F = Vec3(
    W * 0.08 * Math.cos(D) * m / 3600,
    0,
    W * 0.08 * Math.sin(D) * m / 3600
  );

  let v = v0;
  let h = h0;

  let along_the_ground = false;

  const ps = [
    Vec3( r,  0,  0),
    Vec3( 0,  r,  0),
    Vec3( 0,  0,  r),
    Vec3(-r,  0,  0),
    Vec3( 0, -r,  0),
    Vec3( 0,  0, -r),
  ];

  while (true) {
    let vout;

    if (along_the_ground) {
      vout = v.add(g);
    } else {
      const v_ = v.add(g).add(F.scale(1 / m));
      vout = v_.sub(v_.scale(k).scale(1 / m));
    }

    let hout = h.add(vout);

    const arrs = qt.target(
      Math.min(h.x, hout.x) - r,
      Math.min(h.z, hout.z) - r,
      Math.max(h.x, hout.x) + r,
      Math.max(h.z, hout.z) + r,
    );

    let collision = false;

    for (const i of arrs) {
      const { positions: [A, B, C], e, d } = stage.triangle(i);
      const n = B.sub(A).cross(C.sub(A)).normalize();

      let min = { t: 1e9, O: null };
      for (const p of ps) {
        const H = h.add(p);
        const I = hout.add(p);
        if (H.sub(A).dot(n) <= 0 || I.sub(A).dot(n) >= 0) {
          continue;
        }
        const P = intersection_of_plane_and_line(A, B, C, H, I);
        if (!triangle_contains_point(A, B, C, P)) {
          continue;
        }
        const t = P.sub(H).length() / I.sub(H).length();
        if (t < min.t) min = { t, O: P.sub(p) };
      }
      if (min.t === 1e9) continue;
      
      collision = true;

      const V = vout.inverse().rotate(n, 180);
      const W = n.scale(V.dot(n));              // 速度を2つのベクトルに分解する
      const U = V.sub(W);
      const u = U.normalize();

      if (!along_the_ground) {
        along_the_ground = true;
        vout = W.scale(e).add(U);
      } else {
        const N = n.scale(g.dot(n));            // 垂直抗力
        const D = u.scale(N.length() * d);      // 動摩擦力
        if (U.sub(D).dot(u) < 0) {              // 動摩擦力が大きすぎて U 方向の速度がマイナスになれば
          vout = W.scale(e);                    // U 方向の速度を 0 にする
        } else {
          vout = W.scale(e).add(U.sub(D));
        }
      }

      hout = min.O.add(vout.scale(1 - min.t));
      break;
    }

    if (!collision) {
      along_the_ground = false;
    }

    yield hout;

    v = vout, h = hout;
  }
}
