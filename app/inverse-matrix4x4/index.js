import { mat4 } from "/module/gl-matrix/index.js";

window.addEventListener("DOMContentLoaded", () => {
  const inputs = Array.from(document.querySelectorAll("#src input"));
  const outputs = Array.from(document.querySelectorAll("#dest input"));
  const message = document.getElementById("message");

  const change = () => {
    const m = mat4.fromValues(...inputs.map(ipt => Number(ipt.value)));
    if (mat4.determinant(m) === 0) {
      message.textContent = "逆行列は存在しません！";
      outputs.forEach(output => output.value = "");
      return;
    }
    message.textContent = "";
    const o = mat4.create();
    mat4.invert(o, m);
    o.forEach((v, i) => outputs[i].value = v);
  };

  inputs.forEach(input => {
    input.addEventListener("keyup", change);
  });

  outputs.forEach(output => {
    output.readOnly = true;
  });

  {
    const init_data = [
      1, 1, 1, 1,
      1, 1, 1, 1,
      1, 1, 1, 1,
      1, 1, 1, 1,
    ];
    init_data.forEach((v, i) => inputs[i].value = v);
    change();
  }
});
