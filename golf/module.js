import { Polygon } from "/lib/webgl.js";

export function rect(x, y, c = [1, 1, 1]) {
  const data = [], index = [];
  data.push({ position: [0, 0, 0], color: c, normal: [0, 0, 1] });
  data.push({ position: [x, 0, 0], color: c, normal: [0, 0, 1] });
  data.push({ position: [0, y, 0], color: c, normal: [0, 0, 1] });
  data.push({ position: [x, y, 0], color: c, normal: [0, 0, 1] });
  index.push(0, 1, 2);
  index.push(2, 1, 3);
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
      data.push({ position: [x, y, z], color: c, normal: [x, y, z] });
    }
  }
  for (let i = 0; i < row; i++) {
    for (let j = 0; j < col; j++) {
      const std1 = i * (col + 1) + j;
      const std2 = (i + 1) * (col + 1) + j;
      index.push(std1, std1 + 1, std2);
      index.push(std1 + 1, std2 + 1, std2);
    }
  }
  return new Polygon(data, index);
}

export function display_bar(ctx, power, now) {
  const INNER_Y = 417;
  const IMPACT_X = 140;
  const IMPACT_W = 6;
  const UNIT = 36;

  ctx.fillStyle = "rgb(100, 100, 100)";
  ctx.fillRect(100, 412, 417, 32);
  ctx.fillStyle = "rgb(206, 206, 206)";
  ctx.fillRect(101, 413, 415, 30);

  {
    const g = ctx.createLinearGradient(0, INNER_Y, 0, INNER_Y + 10);
    g.addColorStop(0, "rgb(0, 94, 132)");
    g.addColorStop(1, "rgb(21, 66, 85)");
    ctx.fillStyle = g;
    ctx.fillRect(105, INNER_Y, UNIT * 11, 10);
  }
  {
    const g = ctx.createLinearGradient(0, INNER_Y, 0, INNER_Y + 10);
    g.addColorStop(0, "rgb(22, 52, 50)");
    g.addColorStop(1, "rgb(28, 42, 45)");
    ctx.fillStyle = g;
    ctx.fillRect(105, INNER_Y, UNIT, 10);
  }
  {
    const g = ctx.createLinearGradient(0, INNER_Y, 0, INNER_Y + 10);
    g.addColorStop(0, "rgb(7, 217, 255)");
    g.addColorStop(1, "rgb(17, 145, 234)");
    ctx.fillStyle = g;
    const p = power < 0 ? 0 : power;
    ctx.fillRect(IMPACT_X + 1, INNER_Y, UNIT * 10 * p / 100, 10);
  }
  {
    const g = ctx.createLinearGradient(0, INNER_Y, 0, INNER_Y + 10);
    g.addColorStop(0, "rgb(252, 81, 255)");
    g.addColorStop(1, "rgb(251, 71, 252)");
    ctx.fillStyle = g;
    ctx.fillRect(IMPACT_X - 18, INNER_Y, UNIT, 10);
  }

  {
    ctx.fillStyle = "rgba(0, 0, 0, 0.75)";
    ctx.fillRect(105, INNER_Y, UNIT * 11, 1);
    ctx.fillStyle = "rgba(0, 0, 0, 0.15)";
    ctx.fillRect(105, INNER_Y, UNIT * 11, 2);
  }

  for (let i = 0; i < 9; i++) {
    const x = IMPACT_X + (i + 1) * UNIT;
    ctx.fillStyle = "rgb(10, 10, 10)";
    if (i === 4) {
      ctx.fillRect(x, INNER_Y, 1, 10);
    } else {
      ctx.fillRect(x, INNER_Y + 5, 1, 5);
    }
  }
  {
    ctx.fillStyle = "rgb(255, 255, 255)";
    ctx.fillRect(IMPACT_X - IMPACT_W / 2, 417, IMPACT_W, 10);
  }
  {
    const c = Math.round(IMPACT_X + UNIT * 10 * now / 100);
    ctx.fillStyle = "rgb(2, 2, 2)";
    ctx.fillRect(c - 4, 414, 8, 16);
    ctx.fillStyle = "rgb(233, 233, 233)";
    ctx.fillRect(c - 3, 415, 6, 14);
  }
}

export function create_ground() {
  const rects = [];
  const R = 2000;
  const C = 50;
  for (let i = 0; i < C; i++) {
    for (let j = 0; j < C; j++) {
      let color = [0.6, 0.6, 0.6];
      if ((i + j) % 2 === 0) {
        color = [60 / 255, 179 / 255, 113 / 255];
      }
      rects.push(rect(R / C, R / C, color).translate(R / C * i, R / C * j, 0));
    }
  }

  return rects.reduce((a, c) => a.add(c)).rotate([1, 0, 0], -90.0).translate(-R / 2, -10, R / 2);
}

export function display_distance(ctx, dist) {
  const str = (Math.round(dist * 100) / 100).toFixed(2) + " y";

  ctx.font = "bold 32px sans-serif";
  ctx.textAlign = "center";
  ctx.textBaseline = "middle";
  ctx.fillStyle = "red";
  ctx.strokeStyle = "white";
  ctx.fillText(str, ctx.canvas.width / 2, ctx.canvas.height / 2 - 60);
  ctx.strokeText(str, ctx.canvas.width / 2, ctx.canvas.height / 2 - 60);
}

export function xz_distance(a, b) {
  return Math.sqrt(Math.pow(a.x - b.x, 2) + Math.pow(a.z - b.z, 2));
}
