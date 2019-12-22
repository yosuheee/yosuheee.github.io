export function program(gl, vss, fss) {
  const prg = gl.createProgram();
  const list = [
    { type: gl.VERTEX_SHADER, source: vss },
    { type: gl.FRAGMENT_SHADER, source: fss },
  ];
  list.forEach(({ type, source }) => {
    const shader = gl.createShader(type);
    gl.shaderSource(shader, source);
    gl.compileShader(shader);
    gl.attachShader(prg, shader);
  });
  gl.linkProgram(prg);
  gl.useProgram(prg);
  return prg;
}

export const VERTEX_SOURCE = `
attribute vec3 position;
attribute vec3 color;
attribute vec3 normal;
uniform mat4 matrix;
varying vec3 v_color;
varying vec3 v_normal;

void main(void) {
  v_color = color;
  v_normal = normalize(mat3(matrix) * normal);
  gl_Position = matrix * vec4(position, 1.0);
}
`;

export const FRAGMENT_SOURCE = `
precision mediump float;
uniform vec3 light;
varying vec3 v_color;
varying vec3 v_normal;

void main(void) {
  gl_FragColor = vec4(v_color, 1.0);
  gl_FragColor.rgb *= clamp(dot(light, v_normal) / 5.0, 0.0, 0.2) + 0.9;
}
`;
