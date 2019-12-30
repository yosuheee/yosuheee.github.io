import { range } from "./util";

export class UnionFind {
  par: number[];
  siz: number[];
  constructor(n: number) {
    this.par = range(n).map(_ => -1);
    this.siz = range(n).map(_ => 1);
  }
  root(i: number) {
    if (this.par[i] === -1) return i;
    return this.par[i] = this.root(this.par[i]);
  }
  unite(i: number, j: number) {
    if (this.root(i) === this.root(j)) {
      return false;
    }
    this.siz[this.root(i)] += this.siz[this.root(j)];
    this.par[this.root(j)] = this.root(i);
    return true;
  }
  same(i: number, j: number) {
    return this.root(i) === this.root(j);
  }
  size(i: number) {
    return this.siz[this.root(i)];
  }
}
