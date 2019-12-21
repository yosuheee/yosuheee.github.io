export async function sleep(msec) {
  return new Promise(resolve => window.setTimeout(resolve, msec));
}
