export class QuadTree {
  constructor(L, R, U, D, n) {
    this.L = L;
    this.W = R - L;
    this.U = U;
    this.H = D - U;
    this.n = n;
    const arr = [];
    const len = (Math.pow(4, n + 1) - 1) / (4 - 1);
    for (let i = 0; i < len; i++) arr[i] = [];
    this.arr = arr;
    this.len = len;
  }
  register(o, x1, y1, x2, y2) {
    const index = this.object_index(x1, y1, x2, y2);
    console.assert(index.level !== -1);
    this.arr[this.array_index(index)].push(o);
  }
  search(x1, y1, x2, y2) {
    let result = [];
    let index = this.object_index(x1, y1, x2, y2);
    while (index.level >= 0) {
      const i = this.array_index(index);
      result.push(this.arr[i]);
      index = this.parent(index);
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
    return index + (Math.pow(4, level) - 1) / (4 - 1);
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
