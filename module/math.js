export function power(a, n) {
  let acc = 1;
  while (n) {
    if (n & 1) acc *= a;
    a *= a;
    n >>= 1;
  }
  return acc;
}

export function sum_of_geometric_progression(a, r, n) {
  return a * (power(r, n) - 1) / (r - 1);
}
