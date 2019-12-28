describe("Array.prototype.concat()", () => {
  test("pure", () => {
    const a = [1, 2, 3];
    const b = [4, 5, 6];
    const c = a.concat(b);
    expect(a).toEqual([1, 2, 3]);
    expect(b).toEqual([4, 5, 6]);
    expect(c).toEqual([1, 2, 3, 4, 5, 6]);
  });
});
