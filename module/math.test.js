import { power, sum_of_geometric_progression } from "./math.js";

describe("power()", () => {
  test("standard", () => {
    expect(power(5,  3)).toBe(125);
    expect(power(2, 24)).toBe(16777216);
    expect(power(1,  4)).toBe(1);
  });

  test("zero", () => {
    expect(power(0, 5)).toBe(0);
    expect(power(5, 0)).toBe(1);
    expect(power(0, 0)).toBe(1);
  });
});

describe("sum_of_geometric_progression()", () => {
  test("standard", () => {
    expect(sum_of_geometric_progression(1, 4, 3)).toBe(21);
    expect(sum_of_geometric_progression(2, 4, 2)).toBe(10);
  });

  test("zero", () => {
    expect(sum_of_geometric_progression(100, 100, 0)).toBe(0);
  });
});
