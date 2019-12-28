import { UnionFind } from "./union-find.js";

describe("UnionFind", () => {
  test("standard", () => {
    const uf = new UnionFind(8);
    expect(uf.same(0, 1)).toBeFalsy();
    uf.unite(0, 1);
    expect(uf.same(0, 1)).toBeTruthy();
    expect(uf.same(0, 2)).toBeFalsy();
    expect(uf.same(1, 2)).toBeFalsy();
    uf.unite(0, 2);
    expect(uf.same(0, 2)).toBeTruthy();
    expect(uf.same(1, 2)).toBeTruthy();
    expect(uf.size(0)).toBe(3);
    expect(uf.size(1)).toBe(3);
    expect(uf.size(2)).toBe(3);
    expect(uf.par).toEqual([-1, 0, 0, -1, -1, -1, -1, -1]);
  });
});
