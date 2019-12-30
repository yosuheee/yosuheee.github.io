import { V3, M4, triangle_contains_point,
         intersection_of_plane_and_line, 
         Mat4 } from "./index.js";

expect.extend({
  toEqualVec3(received, expected) {
    const pass = received.equals(expected);
    if (pass) {
      return { message: () => "", pass };
    } else {
      return {
        message: () =>
          `expected V3(${expected.primitive()}) not to be V3(${received.primitive()})`,
        pass
      };
    }
  }
});

describe("V3", () => {
  const V = V3(1, 2, 3);
  const U = V3(4, 5, 6);

  test("equals()", () => {
    expect(V).toEqualVec3(V3(1, 2, 3));
    expect(V).not.toEqualVec3(V3(1, 2, 4));
  });

  test("add()", () => {
    expect(V.add(U)).toEqualVec3(V3(5, 7, 9));
  });

  test("sub()", () => {
    expect(U.sub(V)).toEqualVec3(V3(3, 3, 3));
  });

  test("inverse()", () => {
    expect(V.inverse()).toEqualVec3(V3(-1, -2, -3));
  });

  test("is_zero()", () => {
    expect(V.is_zero()).toBeFalsy();
    expect(V3(0, 0, 0).is_zero()).toBeTruthy();
  });

  test("primitive()", () => {
    expect(V.primitive()).toEqual([1, 2, 3]);
  });

  test("dot()", () => {
    const A = V3(2, -5,  3);
    const B = V3(1, -6, -4);
    expect(A.dot(B)).toBe(20);
  });

  test("cross()", () => {
    {
      const A = V3(1, -4, -2);
      const B = V3(2,  5,  1);
      expect(A.cross(B)).toEqualVec3(V3( 6, -5,  13));
      expect(B.cross(A)).toEqualVec3(V3(-6,  5, -13));
    }
    {
      const A = V3(2, 4, -3);
      const B = V3(3, 8, -5);
      expect(A.cross(B)).toEqualVec3(V3( 4,  1,  4));
      expect(B.cross(A)).toEqualVec3(V3(-4, -1, -4));
    }
  });

  test("pure", () => {
    const v1 = V3(1, 2, 3);
    const v2 = v1.add(V3(4, 5, 6));
    const v3 = v2.sub(V3(7, 8, 9));
    const v4 = v1.cross(v3);
    const v5 = V3(0, 0, 10000);
    const v6 = v5.normalize();
    const v7 = v6.inverse();
    const v8 = v7.scale(100);
    const d1 = v1.dot(v4);
    expect(v1).toEqualVec3(V3(1, 2, 3));
    expect(v2).toEqualVec3(V3(5, 7, 9));
    expect(v3).toEqualVec3(V3(-2, -1, 0));
    expect(v4).toEqualVec3(V3(3, -6, 3));
    expect(v5).toEqualVec3(V3(0, 0, 10000));
    expect(v6).toEqualVec3(V3(0, 0, 1));
    expect(v7).toEqualVec3(V3(0, 0, -1));
    expect(v8).toEqualVec3(V3(0, 0, -100));
    expect(d1).toBe(0);
  });
});

describe("M4", () => {
  test("constructor()", () => {
    const data = [
      1, 0, 0, 4,
      0, 2, 0, 5,
      0, 0, 3, 6,
      0, 0, 0, 1,
    ];
    const M = M4(data);
    expect(M.primitive()).toEqual(data);
  });

  test("invert()", () => {
    const data = [
      -1,  0,  1,  1,
       0,  1,  0, -1,
       1, -1,  1,  1,
       0,  0, -1,  0,
    ];
    const expected = [
       0,  1,  1,  1,
       1,  2,  1,  2,
       0,  0,  0, -1,
       1,  1,  1,  2,
    ]
    M4(data).invert().primitive().forEach((v, i) => {
      expect(v).toBeCloseTo(expected[i]);
    });
  });
})

describe("triangle_contains_point()", () => {
  const A = V3( 0,  0,  0);
  const B = V3(10,  0,  0);
  const C = V3( 0, 10,  0);
  const AB = B.sub(A);
  const BC = C.sub(B);
  const CA = A.sub(C);
  const exec = Q => triangle_contains_point(A, B, C, Q);

  test("standard", () => {
    expect(exec(V3( 2,  5,  0))).toBeTruthy();
    expect(exec(V3(-1,  0,  0))).toBeFalsy();
    expect(exec(V3( 0, -1,  0))).toBeFalsy();
    expect(exec(V3( 5,  6,  0))).toBeFalsy();
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
    expect(exec(A.add(AB.scale(0.5).rotate(V3(0, 0, 1), 0.1)))).toBeTruthy();
    expect(exec(B.add(BC.scale(0.5).rotate(V3(0, 0, 1), 0.1)))).toBeTruthy();
    expect(exec(C.add(CA.scale(0.5).rotate(V3(0, 0, 1), 0.1)))).toBeTruthy();
  });

  test("outside near the line", () => {
    expect(exec(A.add(AB.scale(0.5).rotate(V3(0, 0, 1), -0.1)))).toBeFalsy();
    expect(exec(B.add(BC.scale(0.5).rotate(V3(0, 0, 1), -0.1)))).toBeFalsy();
    expect(exec(C.add(CA.scale(0.5).rotate(V3(0, 0, 1), -0.1)))).toBeFalsy();
  });

  test("bugfix", () => {
    const A = V3(-50, 5, 0);
    const B = V3(-100, 3.7187728881835938, 0);
    const C = V3(-50, 5, 50);
    const Q = V3(443.86936557241273, 17.655176417338865, 0);
    expect(triangle_contains_point(A, B, C, Q)).toBeFalsy();
  });
});

describe("intersection_of_plane_and_line()", () => {
  test("parallel", () => {
    const A = V3(0, 0, 0);
    const B = V3(0, 1, 0);
    const C = V3(1, 0, 0);
    const D = V3(0, 0, 1);
    const E = V3(1, 0, 1);
    const r = intersection_of_plane_and_line(A, B, C, D, E);
    expect(r.x).toBeNaN();
    expect(r.y).toBeNaN();
    expect(r.z).toBeNaN();
  });
});

describe("mathematics", () => {
  test("encode by matrix and decode by inverse matrix", () => {
    {
      const a = V3(1, 2, 3);
      const e = Mat4.ortho(-1, 1, -1, 1, -1, 1);
      const b = a.transform(e);
      const c = b.transform(e.invert());
      expect(c.x).toBeCloseTo(a.x);
      expect(c.y).toBeCloseTo(a.y);
      expect(c.z).toBeCloseTo(a.z);
    }
    {
      const a = V3(1, 2, 3);
      const e = Mat4.lookAt(V3(0, 0, 10), V3(0, 0, 0), V3(0, 1, 0));
      const b = a.transform(e);
      const c = b.transform(e.invert());
      expect(c.x).toBeCloseTo(a.x);
      expect(c.y).toBeCloseTo(a.y);
      expect(c.z).toBeCloseTo(a.z);
    }
  });
});
