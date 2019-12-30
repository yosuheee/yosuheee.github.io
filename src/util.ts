export async function sleep(msec: number) {
  return new Promise(resolve => window.setTimeout(resolve, msec));
}

export function range(start: number, length?: number) {
  if (length == null) {
    length = start;
    start = 0;
  }
  const arr: number[] = [];
  for (let i = start; i < start + length; i++) {
    arr.push(i);
  }
  return arr;
}
