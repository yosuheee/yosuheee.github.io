import { range } from "./util";

export class UnionFind {
  constructor(n) {
    this.par = range(n).map(_ => -1);
    this.siz = range(n).map(_ => 1);
  }
  root(i) {
    if (this.par[i] === -1) return i;
    return this.par[i] = this.root(this.par[i]);
  }
  unite(i, j) {
    if (this.root(i) === this.root(j)) {
      return false;
    }
    this.siz[this.root(i)] += this.siz[this.root(j)];
    this.par[this.root(j)] = this.root(i);
    return true;
  }
  same(i, j) {
    return this.root(i) === this.root(j);
  }
  size(i) {
    return this.siz[this.root(i)];
  }
}
