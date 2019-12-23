import { Polygon } from "./polygon.js";
import { Vec3 } from "./geometry.js";

describe("Polygon", () => {
  describe("add()", () => {
    test("the larger array is concatenated", () => {
      const p_large = new Polygon([
        { position: Vec3(1, 1, 1), color: [1, 1, 1] },
        { position: Vec3(1, 2, 1), color: [1, 1, 1] },
        { position: Vec3(2, 1, 1), color: [1, 1, 1] },
        { position: Vec3(2, 2, 1), color: [1, 1, 1] },
      ], [[0, 1, 2], [1, 2, 3]]);
      const p_small = new Polygon([
        { position: Vec3(0, 0, 0), color: [1, 1, 1] },
        { position: Vec3(0, 1, 0), color: [1, 1, 1] },
        { position: Vec3(1, 0, 0), color: [1, 1, 1] },
      ], [[0, 1, 2]]);
      const pl = p_large.add(p_small);
      const pm = p_small.add(p_large);
      expect(pl.data[0].position.primitive()).toEqual([1, 1, 1]);
      expect(pm.data[0].position.primitive()).toEqual([1, 1, 1]);
    });
  });

  describe("primitive()", () => {
    test("normal", () => {
      const p = new Polygon([
        { position: Vec3(1, 1, 1), color: [1, 1, 1] },
        { position: Vec3(0, 1, 1), color: [1, 1, 1] },
        { position: Vec3(1, 1, 0), color: [1, 1, 1] },
        { position: Vec3(1, 0, 1), color: [1, 1, 1] },
      ], [[0, 2, 1], [0, 1, 3], [0, 3, 2]]);
      const { normal } = p.primitive();
      const n = Vec3(1, 1, 1).normalize();
      expect(normal[0]).toBeCloseTo(n.x);
      expect(normal[1]).toBeCloseTo(n.y);
      expect(normal[2]).toBeCloseTo(n.z);
    });
  });
});
