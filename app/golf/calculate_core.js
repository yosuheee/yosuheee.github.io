import { V3,
         intersection_of_plane_and_line,
         triangle_contains_point } from "/module/math/geometry/index.js";

function next_position(stage, qtree, points, v, h, t, g, F, k, r, along_the_ground) {
  const vout = (() => {
    if (t !== 1) {
      return v;
    } else if (along_the_ground) {
      return v.add(g);
    } else {
      return v.add(g).add(F).sub(v.scale(k));
    }
  })();
  const hout = h.add(vout.scale(t));

  const targets = qtree.target(
    Math.min(h.x, hout.x) - r,
    Math.min(h.z, hout.z) - r,
    Math.max(h.x, hout.x) + r,
    Math.max(h.z, hout.z) + r,
  );

  const colls = [];
  for (const i of targets) {
    const { positions: [A, B, C], e, d } = stage.triangle(i);
    const n = B.sub(A).cross(C.sub(A)).normalize();

    let min = { t: 1e9, O: null };
    for (const p of points) {
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
    if (min.t !== 1e9) colls.push({ ...min, n, e, d });
  }

  if (colls.length === 0) {
    return { tout: t, vout, hout, collision: false };
  }

  const { O, t: tout, n, e, d } = colls.reduce((a, c) => a.t < c.t ? a : c);

  return {
    tout,
    vout: (() => {
      const V = vout.inverse().rotate(n, 180);
      const W = n.scale(V.dot(n));
      const U = V.sub(W);
      const u = U.normalize();

      if (!along_the_ground) {
        return W.scale(e).add(U);
      }
      const N = n.scale(g.dot(n));          // 垂直抗力
      const D = u.scale(N.length() * d);    // 動摩擦力
      if (U.sub(D).dot(u) < 0) {            // 動摩擦力が大きすぎて U 方向の速度がマイナスになれば
        return W.scale(e);                  // U 方向の速度を 0 にする
      } else {
        return W.scale(e).add(U.sub(D));
      }
    })(),
    hout: O,
    collision: true,
  };
}

export function* make_positions(v0, h0, stage, qtree, {
  r = 0.04267,      // ボールの半径
  m = 0.04593,      // ボールの質量
  wind_power = 0,   // 風の強さ
  wind_angle = 0,   // 風の角度
  k = 0.003,        // 空気抵抗係数
}) {
  const g = V3(0, -9.8 / 3600, 0);
  const F = V3(
    wind_power * 0.08 * Math.cos(Math.PI * wind_angle / 180) * m / 3600,
    0,
    wind_power * 0.08 * Math.sin(Math.PI * wind_angle / 180) * m / 3600
  ).scale(1 / m);

  const points = [
    V3( r,  0,  0),
    V3( 0,  r,  0),
    V3( 0,  0,  r),
    V3(-r,  0,  0),
    V3( 0, -r,  0),
    V3( 0,  0, -r),
  ];
  
  let v = v0;
  let h = h0;
  let along_the_ground = false;

  while (true) {
    let tacc = 0;
    let coll = false;

    while (tacc < 1) {
      const { tout, vout, hout, collision } =
        next_position(stage, qtree, points, v, h, 1 - tacc, g, F, k, r, along_the_ground);
      v = vout;
      h = hout;
      tacc += tout;
      if (collision) coll = true;
    }

    along_the_ground = coll;

    yield h;
  }
}
