import { power, sum_of_geometric_progression } from "./math/index.js";
import { grid_rect_collision_lt } from "./math/geometry/index.js";

export class QuadTree {
  constructor(L, U, R, D, n) {
    this.L = L;
    this.U = U;
    this.W = R - L;
    this.H = D - U;
    this.n = n;
    const arr = [];
    const len = sum_of_geometric_progression(1, 4, n + 1);
    for (let i = 0; i < len; i++) arr[i] = [];
    this.arr = arr;
  }
  register(o, x1, y1, x2, y2) {
    const index = this.object_index(x1, y1, x2, y2);
    console.assert(~index.level);
    this.arr[this.array_index(index)].push(o);
  }
  target(x1, y1, x2, y2) {
    const result = [];
    const stack = [{ level: 0, index: 0, o: { x: this.L, y: this.U } }];
    while (stack.length) {
      const { level, index, o } = stack.pop();
      result.push(...this.arr[this.array_index({ level, index })]);

      if (level === this.n) continue;

      const w = this.W / power(2, level + 1);
      const h = this.H / power(2, level + 1);
      for (let i = 0; i < 4; i++) {
        const sx = o.x + ((i & 1) ? w : 0);
        const sy = o.y + ((i & 2) ? h : 0);
        const tx = sx + w;
        const ty = sy + h;
        if (!grid_rect_collision_lt(x1, y1, x2, y2, sx, sy, tx, ty)) {
          continue;
        }
        stack.push({
          level: level + 1, index: index * 4 + i, o: { x: sx, y: sy }
        });
      }
    }
    return result;
  }
  object_index(x1, y1, x2, y2) {
    const lu = { x: Math.floor((x1 - this.L) / this.W * Math.pow(2, this.n)),
                 y: Math.floor((y1 - this.U) / this.H * Math.pow(2, this.n)) };
    const rd = { x: Math.floor((x2 - this.L) / this.W * Math.pow(2, this.n)),
                 y: Math.floor((y2 - this.U) / this.H * Math.pow(2, this.n)) };
    if (lu.x < 0 || rd.x >= Math.pow(2, this.n) ||
        lu.y < 0 || rd.y >= Math.pow(2, this.n)) {
      return { level: -1, index: 0 };
    }
    const a = bit_separate(lu.x) | (bit_separate(lu.y) << 1);
    const b = bit_separate(rd.x) | (bit_separate(rd.y) << 1);
    const xor = a ^ b;
    let level = this.n;
    for (let i = 0; i < this.n; i++) {
      const mask = 3 << ((this.n - i - 1) * 2);
      if (xor & mask) { level = i; break; }
    }
    const index = a >> ((this.n - level) * 2);
    return { level, index };
  }
  array_index({ level, index }) {
    return index + sum_of_geometric_progression(1, 4, level);
  }
  parent({ level, index }) {
    return { level: level - 1, index: Math.floor(index / 4) };
  }
}

function bit_separate(n) {
  n = (n | (n << 8)) & 0x00ff00ff;
  n = (n | (n << 4)) & 0x0f0f0f0f;
  n = (n | (n << 2)) & 0x33333333;
  return (n | (n << 1)) & 0x55555555;
}
