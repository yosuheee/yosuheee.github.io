import { Vec3,
         intersection_of_plane_and_line,
         triangle_contains_point } from "/lib/geometry.js";
import { Env } from "./game.js";
import { xz_distance } from "./module.js";

export const calculate = async Game => {
  const h0 = Game.world.positions.slice(-1)[0];

  const func = (positions, prev, resolve) => {
    const h = positions.next().value;
    if (h.equals(prev)) {
      resolve(true);
      return;
    }
    if (h.sub(prev).length() < 0.001) {
      resolve(true);
      return;
    }
    if (h.y < -300) {
      resolve(false);
      return;
    }
    Game.world.positions.push(h);
    Game.distance.xz = xz_distance(h0, h) / 0.9144;

    Game.camera.center = h;
    Game.camera.position = h.add(Vec3(-3, 1, 0).rotate(Vec3(0, 1, 0), Game.hit.angle));

    window.setTimeout(() => func(positions, h, resolve), 0);
  };
  
  return new Promise(resolve => {
    const p = Game.bar.power * 2 / 100;
    const xz = p * Math.cos(Math.PI / 180 * 30);
    const y  = p * Math.sin(Math.PI / 180 * 30);
    const v = Vec3(
      xz *  Math.cos(Math.PI / 180 * Game.hit.angle),
      y,
      xz * -Math.sin(Math.PI / 180 * Game.hit.angle),
    );
    const positions = make_positions(v, h0, Game.world.stage, Game.world.qtree, {
      r: Env.ball.radius,
      W: 0,
      D: Math.PI / 180 * 0,
    });
    func(positions, h0, resolve);
  });
};

function* make_positions(v0, h0, stage, qt, {
  r = 0.04267,            // ボールの半径
  m = 0.04593,            // ボールの質量
  W = 0,                  // 風の強さ
  D = Math.PI / 180 * 0,  // 風の角度
  e = 0.3,                // 反発係数
  k = 0.00015,            // 空気抵抗係数
  d = 0.30,               // 動摩擦係数
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

  while (true) {
    let vout;

    if (along_the_ground) {
      vout = v.add(g);
    } else {
      const v_ = v.add(g).add(F.scale(1 / m));
      vout = v_.sub(v_.scale(k).scale(1 / m));
    }

    let hout = h.add(vout);

    const arrs = qt.search(
      Math.min(h.x, hout.x),
      Math.min(h.z, hout.z),
      Math.max(h.x, hout.x),
      Math.max(h.z, hout.z),
    );

    let collision = false;

    for (const arr of arrs) for (const i of arr) {
      const [A, B, C] = stage.triangle(i);

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
      if (!(0 <= s && s <= 1)) {
        continue;
      }

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
