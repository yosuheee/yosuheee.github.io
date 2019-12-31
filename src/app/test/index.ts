import Vue from "vue";
import { vec3 } from "gl-matrix";

window.addEventListener("DOMContentLoaded", () => {
  const button = document.createElement("button");
  button.innerText = "button";
  button.addEventListener("click", e => {
    const v = vec3.fromValues(e.clientX, e.clientY, 0);
    console.log(v);
  });
  document.getElementById("container").appendChild(button);
});

const app = new Vue({
  el: "#app",
  data: {
    message: "anaconda"
  }
});

const app2 = new Vue({
  el: "#app-2",
  data: {
    message: new Date().getTime(),
  }
});
