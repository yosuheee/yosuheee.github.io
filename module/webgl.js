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
    if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
      console.error(gl.getShaderInfoLog(shader));
    }
    gl.attachShader(prg, shader);
  });
  gl.linkProgram(prg);
  gl.useProgram(prg);
  return prg;
}

export function uniform(gl, prg, type, name, value) {
  const loc = gl.getUniformLocation(prg, name);
  switch (type) {
    case "float": return gl.uniform1f(loc, value);
    case "mat4" : return gl.uniformMatrix4fv(loc, false, value);
    case "vec2" : return gl.uniform2fv(loc, value);
    case "vec3" : return gl.uniform3fv(loc, value);
    case "vec4" : return gl.uniform4fv(loc, value);
    default: throw new Error(`unknown type name '${type}'`);
  }
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
uniform mat4 mvp_matrix;
uniform mat4 inv_matrix;
varying vec4 v_color;
varying vec3 v_normal;

void main(void) {
  v_color = color;
  v_normal = vec3(inv_matrix * vec4(normal, 0.0));
  gl_Position = mvp_matrix * vec4(position, 1.0);
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
