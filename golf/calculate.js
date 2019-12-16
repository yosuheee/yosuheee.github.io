import { Vec3,
         intersection_of_plane_and_line,
         triangle_contains_point } from "/lib/geometry.js";

export function* make_positions(v0, h0, ground, {
  r = 0.04267,            // ボールの半径
  m = 0.04593,            // ボールの質量
  W = 0,                  // 風の強さ
  D = Math.PI / 180 * 0,  // 風の角度
  e = 0.3,                // 反発係数
  k = 0.00015,            // 空気抵抗係数
  d = 0.33,               // 動摩擦係数
}) {
  const g = Vec3(0, -9.8 / 3600, 0);
  const F = Vec3(
    W * 0.08 * Math.cos(D) * m / 3600,
    0,
    W * 0.08 * Math.sin(D) * m / 3600
  );

  let v = v0;
  let h = h0;

  const { position, index } = ground.primitive();

  let along_the_ground = false;

  while (true) {
    let vout;

    if (along_the_ground) {
      vout = v.add(g);
    } else {
      const v_ = v.add(g).add(F.scale(1 / m));
      vout = v_.sub(v_.scale(k).scale(1 / m));
    }

    let hout = h.add(vout);

    let collision = false;

    for (let i = 0; i < index.length; i += 3) {
      const A = Vec3(position.slice(index[i + 0] * 3, index[i + 0] * 3 + 3));
      const B = Vec3(position.slice(index[i + 1] * 3, index[i + 1] * 3 + 3));
      const C = Vec3(position.slice(index[i + 2] * 3, index[i + 2] * 3 + 3));

      const n = B.sub(A).cross(C.sub(A)).normalize();
      if (n.dot(hout.sub(A)) > r) {
        continue;
      }
      
      const P = intersection_of_plane_and_line(A, B, C, h, hout);

      const _OQ = r;
      const _hS = n.dot(h.sub(P));
      const PS = h.add(n.scale(-_hS)).sub(P);
      const Q = PS.scale(_OQ / _hS).add(P);
      if (!triangle_contains_point(A, B, C, Q)) {
        continue;
      }

      const O = h.sub(P).scale(_OQ / _hS).add(P);
      const s = O.sub(h).length() / hout.sub(h).length();
      console.assert(0 <= s && s <= 1);

      const v = vout.scale(-1).rotate(n, 180);
      const w = n.scale(n.dot(v));
      const u = v.sub(w);

      if (along_the_ground) {
        const N = n.scale(-g.dot(n));
        const r = u.scale(-1).normalize();
        const p = r.scale(N.length() * d);
        if (u.add(p).dot(r) > 0) {
          vout = w.scale(e);
        } else {
          vout = w.scale(e).add(u.add(p));
        }
      } else {
        vout = w.scale(e).add(u);
        along_the_ground = true;
      }

      hout = O.add(vout.scale(1 - s));

      collision = true;
      break;
    }

    if (!collision) {
      along_the_ground = false;
    }

    yield hout;

    v = vout;
    h = hout;
  }
}
