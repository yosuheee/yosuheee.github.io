export async function sleep(msec) {
  return new Promise(resolve => window.setTimeout(resolve, msec));
}

export function range(start, length) {
  if (length == null) {
    length = start;
    start = 0;
  }
  const arr = [];
  for (let i = start; i < start + length; i++) {
    arr.push(i);
  }
  return arr;
}
