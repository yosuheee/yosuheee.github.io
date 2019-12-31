export declare class UnionFind {
  constructor(n: number);
  root(i: number): number;
  same(i: number, j: number): boolean;
  size(i: number): number;
  unite(i: number, j: number): boolean;
}
