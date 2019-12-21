import { Vec3 } from "/lib/geometry.js";
import { Polygon } from "/lib/webgl.js";

export function rect(x, y, c = [1, 1, 1]) {
  const data = [], index = [];
  data.push({ position: Vec3(0, 0, 0), color: c });
  data.push({ position: Vec3(x, 0, 0), color: c });
  data.push({ position: Vec3(0, y, 0), color: c });
  data.push({ position: Vec3(x, y, 0), color: c });
  index.push([0, 1, 2]);
  index.push([2, 1, 3]);
  return new Polygon(data, index);
}

export function sphere(radius, c = [1, 1, 1], row = 32, col = 32) {
  const data = [], index = [];
  for (let i = 0; i <= row; i++) {
    const langle = Math.PI / row * i;
    const rad = radius * Math.sin(langle);
    const y = radius * Math.cos(langle);
    for (let j = 0; j <= col; j++) {
      const sangle = Math.PI * 2 / col * j;
      const x = rad * Math.cos(sangle);
      const z = rad * Math.sin(sangle);
      data.push({ position: Vec3(x, y, z), color: c });
    }
  }
  for (let i = 0; i < row; i++) {
    for (let j = 0; j < col; j++) {
      const std1 = i * (col + 1) + j;
      const std2 = (i + 1) * (col + 1) + j;
      index.push([std1, std1 + 1, std2]);
      index.push([std1 + 1, std2 + 1, std2]);
    }
  }
  return new Polygon(data, index);
}
