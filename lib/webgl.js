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
uniform mat4 r_matrix;
varying vec3 v_color;
varying vec3 v_normal;

void main(void) {
  v_color = color;
  v_normal = (r_matrix * vec4(normal, 0.0)).xyz;
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
  gl_FragColor.rgb *= clamp(dot(light, v_normal), 0.1, 1.0);
}
`;
