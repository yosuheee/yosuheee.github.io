import { range } from "./util.js";
import { QuadTree } from "./quad-tree.js";
import { sum_of_geometric_progression, power } from "./math.js";

describe("QuadTree", () => {
  test("constructor()", () => {
    {
      const qt = new QuadTree(2, 3, 5, 8, 5);
      expect(qt.L).toBe(2);
      expect(qt.U).toBe(3);
      expect(qt.W).toBe(3);
      expect(qt.H).toBe(5);
      expect(qt.arr.length).toBe(1365);
    }
    {
      const qt = new QuadTree(0, 0, 32, 32, 3);
      expect(qt.L).toBe(0);
      expect(qt.U).toBe(0);
      expect(qt.W).toBe(32);
      expect(qt.H).toBe(32);
      expect(qt.arr.length).toBe(85);
    }
  });

  const qtree = new QuadTree(0, 0, 8, 8, 2);
  const rlist = [
    [0, 0, 0, 1, 1],
    [1, 1, 2, 3, 3],
    [2, 1, 4, 2, 7],
    [3, 4, 0, 7, 3],
    [4, 6, 6, 7, 7],
    [5, 5, 6, 7, 7],
    [6, 3, 3, 4, 5],
    [7, 0, 0, 7, 7],
    [8, 4, 3, 5, 4],
    [9, 2, 6, 3, 7],
  ];
  rlist.forEach(arr => qtree.register(...arr));

  test("register()", () => {
    expect(qtree.arr).toEqual(morton([
      [6, 7, 8],
      [1], [3],
      [2], [5],
      [0], [ ], [ ], [ ],
      [ ], [ ], [ ], [ ],
      [ ], [ ], [ ], [ ],
      [ ], [9], [ ], [4],
    ]));
  });

  describe("search()", () => {
    const exp_list = [
      { args: [0, 0, 1, 1], exps: [0, 1, 6, 7, 8] },
      { args: [2, 0, 3, 1], exps: [1, 6, 7, 8] },
      { args: [0, 0, 7, 7], exps: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] },
      { args: [2, 6, 3, 7], exps: [2, 6, 7, 8, 9] },
      { args: [6, 6, 7, 7], exps: [4, 5, 6, 7, 8] },
      { args: [4, 4, 5, 5], exps: [5, 6, 7, 8] },
      { args: [0, 0, 3, 3], exps: [0, 1, 6, 7, 8] },
      { args: [2, 2, 3, 3], exps: [1, 6, 7, 8] },
    ];

    test("standard", () => {
      exp_list.forEach(({ args, exps }) => {
        expect(qtree.search(...args).sort()).toEqual(exps);
      });
    });

    test("minus", () => {
      const qt = new QuadTree(-4, -4, 4, 4, 2);
      const li = rlist.map(a => [a[0], a[1] - 4, a[2] - 4, a[3] - 4, a[4] - 4]);
      li.forEach(arr => qt.register(...arr));
      const el = exp_list.map(({ args, exps }) => ({ args: args.map(d => d - 4), exps }));
      el.forEach(({ args, exps }) => {
        expect(qt.search(...args).sort()).toEqual(exps);
      });
    });
  });
});

function morton(arr) {
  const out = range(0, arr.length);
  const n = (() => {
    for (let i = 1; ; i++) {
      const sum = sum_of_geometric_progression(1, 4, i);
      if (sum === arr.length) {
        return i;
      }
      console.assert(sum < arr.length);
    }
  })();
  for (let i = 0; i < n; i++) {
    const offset = sum_of_geometric_progression(1, 4, i);
    for (let j = 0; j < power(4, i); j++) {
      let x = 0, y = 0;
      for (let k = 0; k < i; k++) {
        x <<= 1;
        y <<= 1;
        if ((j >> ((i - k - 1) * 2)) & 1) x += 1;
        if ((j >> ((i - k - 1) * 2)) & 2) y += 1;
      }
      const k = y * power(2, i) + x;
      out[offset + j] = arr[offset + k];
    }
  }
  return out;
}
