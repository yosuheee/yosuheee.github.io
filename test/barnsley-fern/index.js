window.addEventListener("DOMContentLoaded", () => {
  const cvs = document.getElementById("canvas");
  const ctx = cvs.getContext("2d");

  ctx.clearRect(0, 0, cvs.width, cvs.height);
  ctx.fillStyle = "green";

  let x = 0, y = 0;
  const tick = () => {
    requestAnimationFrame(tick);
    ctx.fillRect(
      (1.2 * x + 5) * cvs.width / 10,
      (245 - 24 * y) * cvs.height / 250,
      1, 1
    );

    let xout, yout;
    const percent = Math.floor(Math.random() * 100);
    if (percent === 0) {
      xout = 0;
      yout = 0.16 * y;
    } else if (percent <= 7) {
      xout = 0.2 * x - 0.26 * y;
      yout = 0.23 * x + 0.22 * y + 1.6;
    } else if (percent <= 14) {
      xout = -0.15 * x + 0.28 * y;
      yout = 0.26 * x + 0.24 * y + 0.44;
    } else {
      xout = 0.85 * x + 0.04 * y;
      yout = -0.04 * x + 0.85 * y + 1.6;
    }
    x = xout, y = yout;
  };
  tick();
});
