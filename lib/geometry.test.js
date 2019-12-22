import { Vec3, triangle_contains_point } from "./geometry.js";

describe("Vec3", () => {
  const V = Vec3(1, 2, 3);
  const U = Vec3(4, 5, 6);

  test("equals()", () => {
    expect(V.equals(Vec3(1, 2, 3))).toBeTruthy();
    expect(V.equals(Vec3(1, 2, 4))).toBeFalsy();
  });

  test("add()", () => {
    expect(V.add(U).equals(Vec3(5, 7, 9))).toBeTruthy();
  });

  test("sub()", () => {
    expect(U.sub(V).equals(Vec3(3, 3, 3))).toBeTruthy();
  });

  test("inverse()", () => {
    expect(V.inverse().equals(Vec3(-1, -2, -3))).toBeTruthy();
  });

  test("is_zero()", () => {
    expect(V.is_zero()).toBeFalsy();
    expect(Vec3(0, 0, 0).is_zero()).toBeTruthy();
  });

  test("primitive()", () => {
    expect(V.primitive()).toEqual([1, 2, 3]);
  });
});

describe("triangle_contains_point()", () => {
  const A = Vec3( 0,  0,  0);
  const B = Vec3(10,  0,  0);
  const C = Vec3( 0, 10,  0);
  const AB = B.sub(A);
  const BC = C.sub(B);
  const CA = A.sub(C);
  const exec = Q => triangle_contains_point(A, B, C, Q);

  test("standard", () => {
    expect(exec(Vec3( 2,  5,  0))).toBeTruthy();
    expect(exec(Vec3(-1,  0,  0))).toBeFalsy();
    expect(exec(Vec3( 0, -1,  0))).toBeFalsy();
    expect(exec(Vec3( 5,  6,  0))).toBeFalsy();
  });

  test("on the point", () => {
    expect(exec(A)).toBeTruthy();
    expect(exec(B)).toBeTruthy();
    expect(exec(C)).toBeTruthy();
  });

  test("on the line", () => {
    expect(exec(A.add(AB.scale(0.5)))).toBeTruthy();
    expect(exec(A.add(AB.scale(0.1)))).toBeTruthy();
    expect(exec(B.add(BC.scale(0.7)))).toBeTruthy();
    expect(exec(C.add(CA.scale(0.3)))).toBeTruthy();
  });

  test("inside near the line", () => {
    expect(exec(A.add(AB.scale(0.5).rotate(Vec3(0, 0, 1), 0.1)))).toBeTruthy();
    expect(exec(B.add(BC.scale(0.5).rotate(Vec3(0, 0, 1), 0.1)))).toBeTruthy();
    expect(exec(C.add(CA.scale(0.5).rotate(Vec3(0, 0, 1), 0.1)))).toBeTruthy();
  });

  test("outside near the line", () => {
    expect(exec(A.add(AB.scale(0.5).rotate(Vec3(0, 0, 1), -0.1)))).toBeFalsy();
    expect(exec(B.add(BC.scale(0.5).rotate(Vec3(0, 0, 1), -0.1)))).toBeFalsy();
    expect(exec(C.add(CA.scale(0.5).rotate(Vec3(0, 0, 1), -0.1)))).toBeFalsy();
  });
});
