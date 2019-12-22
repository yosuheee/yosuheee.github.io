import { range } from "./util.js";

describe("range()", () => {
  test("start == 0", () => {
    const arr = range(0, 3);
    expect(arr).toEqual([0, 1, 2]);
  });
  
  test("start != 0", () => {
    const arr = range(1, 3);
    expect(arr).toEqual([1, 2, 3]);
  });
});
