import { Triangles } from "./primitives.js";
import { V3 } from "./math/geometry/index.js";

describe("Triangles", () => {
  describe("add()", () => {
    test("the larger array is concatenated", () => {
      const p_large = new Triangles([
        { position: V3(1, 1, 1), color: [1, 1, 1] },
        { position: V3(1, 2, 1), color: [1, 1, 1] },
        { position: V3(2, 1, 1), color: [1, 1, 1] },
        { position: V3(2, 2, 1), color: [1, 1, 1] },
      ], [[0, 1, 2], [1, 2, 3]]);
      const p_small = new Triangles([
        { position: V3(0, 0, 0), color: [1, 1, 1] },
        { position: V3(0, 1, 0), color: [1, 1, 1] },
        { position: V3(1, 0, 0), color: [1, 1, 1] },
      ], [[0, 1, 2]]);
      const pl = p_large.add(p_small);
      const pm = p_small.add(p_large);
      expect(pl.data[0].position.primitive()).toEqual([1, 1, 1]);
      expect(pm.data[0].position.primitive()).toEqual([1, 1, 1]);
    });
  });

  describe("primitive()", () => {
    test("normal", () => {
      const p = new Triangles([
        { position: V3(1, 1, 1), color: [1, 1, 1] },
        { position: V3(0, 1, 1), color: [1, 1, 1] },
        { position: V3(1, 1, 0), color: [1, 1, 1] },
        { position: V3(1, 0, 1), color: [1, 1, 1] },
      ], [[0, 2, 1], [0, 1, 3], [0, 3, 2]]);
      const { normal } = p.primitive();
      const n = V3(1, 1, 1).normalize();
      expect(normal[0]).toBeCloseTo(n.x);
      expect(normal[1]).toBeCloseTo(n.y);
      expect(normal[2]).toBeCloseTo(n.z);
    });
  });

  describe("flatten()", () => {
    test("standard", () => {
      const p = new Triangles([
        { position: V3(0, 0, 1), color: [1, 1, 1] },
        { position: V3(1, 0, 1), color: [1, 1, 1] },
        { position: V3(0, 1, 1), color: [1, 1, 1] },
        { position: V3(0, 1.0005, 1), color: [1, 1, 1] },
        { position: V3(1, 0.0005, 1), color: [1, 1, 1] },
        { position: V3(1, 1, 1), color: [1, 1, 1] },
      ], [[0, 1, 2], [3, 4, 5]]);
      const new_p = p.flatten();
      expect(new_p.data.length).toBe(4);
      expect(new_p.index).toEqual([[0, 1, 2], [2, 1, 3]]);
    });
  });

  describe("rotate()", () => {
    const p = new Triangles([
      { position: V3(0, 0, 0), color: [1, 1, 1] },
      { position: V3(0, 1, 0), color: [1, 1, 1] },
      { position: V3(1, 0, 0), color: [1, 1, 1] },
      { position: V3(1, 1, 0), color: [1, 1, 1] },
    ], [[0, 1, 2], [1, 2, 3]]);
    const t = p.rotate(V3(1, 1, 0),  45);
    const r = t.rotate(V3(1, 1, 0), -45);
    const { position: a } = p.primitive();
    const { position: b } = r.primitive();
    for (let i = 0; i < 3 * 4; i++) {
      expect(b[i]).toBeCloseTo(a[i]);
    }
  })
});
