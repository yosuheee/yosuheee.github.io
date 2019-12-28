import { Mat4, V3 } from "../lib/geometry.js";
import { range } from "../lib/util.js";

test("translate()", () => {
  {
    const m = Mat4.translate(3, 4, 5).primitive();
    expect(m).toEqual(new Float32Array(transpose([
      1, 0, 0, 3,
      0, 1, 0, 4,
      0, 0, 1, 5,
      0, 0, 0, 1,
    ])));
  }
  {
    const m = Mat4.scale(3, 4, 5).primitive();
    expect(m).toEqual(new Float32Array(transpose([
      3, 0, 0, 0,
      0, 4, 0, 0,
      0, 0, 5, 0,
      0, 0, 0, 1,
    ])));
  }
  {
    const m = Mat4.scale(3, 4, 5).translate(6, 7, 8).primitive();
    expect(m).toEqual(new Float32Array(transpose([
      3, 0, 0, 6,
      0, 4, 0, 7,
      0, 0, 5, 8,
      0, 0, 0, 1,
    ])));
  }
  {
    const m = Mat4.translate(6, 7, 8).scale(3, 4, 5).primitive();
    expect(m).toEqual(new Float32Array(transpose([
      3, 0, 0, 18,
      0, 4, 0, 28,
      0, 0, 5, 40,
      0, 0, 0,  1,
    ])));
  }
});

function transpose(arr) {
  const out = range(16);
  for (let i = 0; i < 4; i++)
  for (let j = 0; j < 4; j++) {
    out[i * 4 + j] = arr[j * 4 + i];
  }
  return out;
}
