import { QuadTree } from "/module/quad-tree.js";
import { V3 } from "/module/math/geometry/index.js";
import { Triangles } from "/module/triangles.js";
import { Env } from "./game.js";
import { intersection_of_plane_and_line, triangle_contains_point } from "/module/math/geometry/index.js";

export function display_bar(ctx, power, now) {
const x = 100, y = 412;

  const IMPACT_W = 6;
  
  const UNIT = 36;

  ctx.fillStyle = "rgb(100, 100, 100)";
  ctx.fillRect(x, y, 417, 32);
  ctx.fillStyle = "rgb(206, 206, 206)";
  ctx.fillRect(x + 1, y + 1, 415, 30);

  {
    const g = ctx.createLinearGradient(0, y + 5, 0, y + 15);
    g.addColorStop(0, "rgb(0, 94, 132)");
    g.addColorStop(1, "rgb(21, 66, 85)");
    ctx.fillStyle = g;
    ctx.fillRect(x + 5, y + 5, UNIT * 11, 10);
  }
  {
    const g = ctx.createLinearGradient(0, y + 5, 0, y + 15);
    g.addColorStop(0, "rgb(22, 52, 50)");
    g.addColorStop(1, "rgb(28, 42, 45)");
    ctx.fillStyle = g;
    ctx.fillRect(x + 5, y + 5, UNIT, 10);
  }
  {
    const g = ctx.createLinearGradient(0, y + 5, 0, y + 15);
    g.addColorStop(0, "rgb(7, 217, 255)");
    g.addColorStop(1, "rgb(17, 145, 234)");
    ctx.fillStyle = g;
    const p = power < 0 ? 0 : power;
    ctx.fillRect(x + 41, y + 5, UNIT * 10 * p / 100, 10);
  }
  {
    const g = ctx.createLinearGradient(0, y + 5, 0, y + 15);
    g.addColorStop(0, "rgb(252, 81, 255)");
    g.addColorStop(1, "rgb(251, 71, 252)");
    ctx.fillStyle = g;
    ctx.fillRect(x + 22, y + 5, UNIT, 10);
  }

  {
    ctx.fillStyle = "rgba(0, 0, 0, 0.75)";
    ctx.fillRect(x + 5, y + 5, UNIT * 11, 1);
    ctx.fillStyle = "rgba(0, 0, 0, 0.15)";
    ctx.fillRect(x + 5, y + 5, UNIT * 11, 2);
  }

  for (let i = 0; i < 9; i++) {
    const p = x + 40 + (i + 1) * UNIT;
    ctx.fillStyle = "rgb(10, 10, 10)";
    if (i === 4) {
      ctx.fillRect(p, y + 5, 1, 10);
    } else {
      ctx.fillRect(p, y + 10, 1, 5);
    }
  }
  {
    ctx.fillStyle = "rgb(255, 255, 255)";
    ctx.fillRect(x + 40 - IMPACT_W / 2, y + 5, IMPACT_W, 10);
  }
  {
    const c = Math.round(x + 40 + UNIT * 10 * now / 100);
    ctx.fillStyle = "rgb(2, 2, 2)";
    ctx.fillRect(c - 4, y + 2, 8, 16);
    ctx.fillStyle = "rgb(233, 233, 233)";
    ctx.fillRect(c - 3, y + 3, 6, 14);
  }
}

export function make_random_stage(r = 50, e = 50) {
  console.assert(r % 2 === 0);
  const data = [], index = [], tridata = [];
  const diff = 5;
  for (let i = 0; i <= r; i++) {
    for (let j = 0; j <= r; j++) {
      const x = i * e;
      const z = j * e;
      let y;
      let color;
      if (i >= r / 2 - 1 && i <= r / 2 + 1 &&
          j >= r / 2 - 1 && j <= r / 2 + 1) {
        y = diff;
        color = [0, 0.3, 0];
      } else {
        y = (Math.random() * diff * 2) - diff;
        color = [Math.random() / 5, (y + 1) / 3, Math.random() / 5];
      }
      data.push({ position: V3(x, y, z), color });
    }
  }
  for (let i = 0; i < r; i++) {
    for (let j = 0; j < r; j++) {
      const s = i * (r + 1) + j;
      const t = (i + 1) * (r + 1) + j;
      index.push([s, s + 1, t + 1]);
      tridata.push({ e: Math.random(), d: Math.random() * 0.5 + 0.25 });
      index.push([t, s, t + 1]);
      tridata.push({ e: Math.random(), d: Math.random() * 0.5 + 0.25 });
    }
  }
  return new Triangles(data, index, tridata).translate(-e * r / 2, 0, -e * r / 2);
}

export function display_distance(ctx, dist) {
  const str = (Math.round(dist * 100) / 100).toFixed(2) + " y";

  ctx.font = "bold 32px sans-serif";
  ctx.textAlign = "center";
  ctx.textBaseline = "middle";
  ctx.fillStyle = "red";
  ctx.strokeStyle = "white";
  ctx.fillText(str, ctx.canvas.width / 2, ctx.canvas.height / 2 - 60);
  ctx.strokeText(str, ctx.canvas.width / 2, ctx.canvas.height / 2 - 60);
}

export function xz_distance(a, b) {
  return Math.sqrt(Math.pow(a.x - b.x, 2) + Math.pow(a.z - b.z, 2));
}

export function make_qtree(stage) {
  const qt = new QuadTree(...(() => {
    const x = { min: 1e9, max: -1 };
    const z = { min: 1e9, max: -1 };
    for (const { positions: [A, B, C] } of stage.triangles()) {
      x.min = Math.min(x.min, ...[A.x, B.x, C.x]);
      z.min = Math.min(z.min, ...[A.z, B.z, C.z]);
      x.max = Math.max(x.max, ...[A.x, B.x, C.x]);
      z.max = Math.max(z.max, ...[A.z, B.z, C.z]);
    }
    return [ x.min, z.min, x.max + 1, z.max + 1 ];
  })(), 5);
  for (const { positions: [A, B, C], index } of stage.triangles()) {
    const x = { min: Math.min(...[A.x, B.x, C.x]),
                max: Math.max(...[A.x, B.x, C.x]) };
    const z = { min: Math.min(...[A.z, B.z, C.z]),
                max: Math.max(...[A.z, B.z, C.z]) };
    qt.register(index, x.min, z.min, x.max, z.max);
  }
  return qt;
}

export function xyz_from_xz(stage, qtree, x, z) {
  const arrs = qtree.target(x, z, x, z);

  for (const i of arrs) {
    const [A, B, C] = stage.triangle(i).positions;
    const D = V3(x, 0, z);
    const E = V3(x, 1, z);
    const P = intersection_of_plane_and_line(A, B, C, D, E);
    if (triangle_contains_point(A, B, C, P)) {
      return P.add(V3(0, Env.ball.radius, 0));
    }
  }
};
