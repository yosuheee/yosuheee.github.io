import { V3, Mat4 } from "../module/math/geometry/index.js";
import { rect } from "../module/triangles.js";

const it = function*(polygon) {
  const { position, normal, index } = polygon.primitive();
  for (let i = 0; i < index.length; i += 3) {
    const a = V3(...[0, 1, 2].map(j => position[index[i + 0] * 3 + j]));
    const b = V3(...[0, 1, 2].map(j => position[index[i + 1] * 3 + j]));
    const c = V3(...[0, 1, 2].map(j => position[index[i + 2] * 3 + j]));
    const n = V3(...[0, 1, 2].map(j => normal[index[i] * 3 + j]));
    yield { a, b, c, n };
  }
};

test("simulate shader program", () => {
  const reverse_light_direction = V3(1, 1, 0).normalize();
  const rects = [
    rect(5, 5).rotate(V3(0, 1, 0),  90).translate(5, 0, 5),
    rect(5, 5).rotate(V3(1, 0, 0), -90).translate(0, 5, 5),
    rect(5, 5).translate(0, 0, 5),
  ];
  const m = Mat4.rotate(V3(0, 1, 0), 90);
  const p = rects.reduce((a, c) => a.add(c));
  const expected = [];
  for (const { n } of it(p.transform(m))) {
    expected.push(n.dot(reverse_light_direction));
  }
  {
    const received = [];
    for (const { n } of (it(p))) {
      received.push(n.dot(reverse_light_direction.transform(m.invert())));
    }
    for (let i = 0; i < expected.length; i++) {
      expect(received[i]).toBeCloseTo(expected[i]);
    }
  }
  {
    const r = Mat4.rotate(V3(0, 1, 0), -90);
    const received = [];
    for (const { n } of (it(p))) {
      received.push(n.dot(reverse_light_direction.transform(r)));
    }
    for (let i = 0; i < expected.length; i++) {
      expect(received[i]).toBeCloseTo(expected[i]);
    }
  }
});
