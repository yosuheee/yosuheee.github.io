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

export function uniform(gl, prg, type, name, value) {
  switch (type) {
    case "mat4": return uniformMatrix4fv(gl, prg, name, value);
    case "vec3": return uniform3fv(gl, prg, name, value);
    case "vec4": return uniform4fv(gl, prg, name, value);
    default: throw new Error(`unknown type name '${type}'`);
  }
}

export function uniformMatrix4fv(gl, prg, name, value) {
  const loc = gl.getUniformLocation(prg, name);
  gl.uniformMatrix4fv(loc, false, value);
}

export function uniform3fv(gl, prg, name, value) {
  const loc = gl.getUniformLocation(prg, name);
  gl.uniform3fv(loc, value);
}

export function uniform4fv(gl, prg, name, value) {
  const loc = gl.getUniformLocation(prg, name);
  gl.uniform4fv(loc, value);
}

export function buffer(gl, type, value) {
  const vbo = gl.createBuffer();
  gl.bindBuffer(type, vbo);
  gl.bufferData(type, value, gl.STATIC_DRAW);
  return vbo;
}

export const VERTEX_SOURCE = `
attribute vec3 position;
attribute vec4 color;
attribute vec3 normal;
uniform mat4 m_matrix;
uniform mat4 r_matrix;
varying vec4 v_color;
varying vec3 v_normal;

void main(void) {
  v_color = color;
  v_normal = (r_matrix * vec4(normal, 0.0)).xyz;
  gl_Position = m_matrix * vec4(position, 1.0);
}
`;

export const FRAGMENT_SOURCE = `
precision mediump float;
uniform vec3 light;
varying vec4 v_color;
varying vec3 v_normal;

void main(void) {
  gl_FragColor = v_color;
  gl_FragColor.rgb *= clamp(dot(light, v_normal), 0.1, 1.0);
}
`;
