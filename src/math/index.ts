export function power(a: number, n: number) {
  let acc = 1;
  while (n) {
    if (n & 1) acc *= a;
    a *= a;
    n >>= 1;
  }
  return acc;
}

export function sum_of_geometric_progression(a: number, r: number, n: number) {
  return a * (power(r, n) - 1) / (r - 1);
}
