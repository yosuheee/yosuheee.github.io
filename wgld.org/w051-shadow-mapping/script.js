const q = new qtnIV();
const qt = q.identity(q.create());

let c;

// マウスムーブイベントに登録する処理
function mouseMove(e){
	var cw = c.width;
	var ch = c.height;
	var wh = 1 / Math.sqrt(cw * cw + ch * ch);
	var x = e.clientX - c.offsetLeft - cw * 0.5;
	var y = e.clientY - c.offsetTop - ch * 0.5;
	var sq = Math.sqrt(x * x + y * y);
	var r = sq * 2.0 * Math.PI * wh;
	if(sq != 1){
		sq = 1 / sq;
		x *= sq;
		y *= sq;
	}
	q.rotate(r, [y, x, 0.0], qt);
}

window.addEventListener("DOMContentLoaded", () => {
  c = document.getElementById("canvas");
  const gl = c.getContext("webgl");
  const e_range = document.getElementById("range");
  const e_radio = document.getElementById("depthBuffer");

	c.addEventListener('mousemove', mouseMove, true);

  const s_prg = (() => {
    const v_shader = create_shader(gl, "svs");
    const f_shader = create_shader(gl, "sfs");
    return create_program(gl, v_shader, f_shader);
  })();
  const d_prg = (() => {
    const v_shader = create_shader(gl, "dvs");
    const f_shader = create_shader(gl, "dfs");
    return create_program(gl, v_shader, f_shader);
  })();

  const s_atts = [
    { location: gl.getAttribLocation(s_prg, "position"), stride: 3 },
    { location: gl.getAttribLocation(s_prg, "normal"),   stride: 3 },
    { location: gl.getAttribLocation(s_prg, "color"),    stride: 4 },
  ]
  const d_atts = [
    { location: gl.getAttribLocation(d_prg, "position"), stride: 3 },
  ];

  const torus_data = torus(64, 64, 1.0, 2.0, [1, 1, 1, 1]);
  const t_position = create_vbo(gl, torus_data.position);
  const t_normal   = create_vbo(gl, torus_data.normal);
  const t_color    = create_vbo(gl, torus_data.color);
  const t_vbo_list = [t_position, t_normal, t_color];
  const t_index    = create_ibo(gl, torus_data.index);

  const position = [
    -1,  0, -1,
     1,  0, -1,
    -1,  0,  1,
     1,  0,  1,
  ];
  const normal = [
    0, 1, 0,
    0, 1, 0,
    0, 1, 0,
    0, 1, 0,
  ];
  const color = [
    0.5, 0.5, 0.5, 1.0,
    0.5, 0.5, 0.5, 1.0,
    0.5, 0.5, 0.5, 1.0,
    0.5, 0.5, 0.5, 1.0,
  ];
  const index = [
    0, 2, 1,
    3, 1, 2,
  ];

  const v_position = create_vbo(gl, position);
  const v_normal   = create_vbo(gl, normal);
  const v_color    = create_vbo(gl, color);
  const v_vbo_list = [v_position, v_normal, v_color];
  const v_index    = create_ibo(gl, index);

  const dt_vbo_list = [t_position];

  const s_uni_location = [
    "mMatrix", "mvpMatrix", "invMatrix", "tMatrix", "lgtMatrix",
    "lightPosition", "texture", "depthBuffer",
  ].map(name => gl.getUniformLocation(s_prg, name));
  const d_uni_location = [
    "mvpMatrix", "depthBuffer"
  ].map(name => gl.getUniformLocation(d_prg, name));

	var m = new matIV();
	var mMatrix    = m.identity(m.create());
	var vMatrix    = m.identity(m.create());
	var pMatrix    = m.identity(m.create());
	var tmpMatrix  = m.identity(m.create());
	var mvpMatrix  = m.identity(m.create());
	var invMatrix  = m.identity(m.create());
	var tMatrix    = m.identity(m.create());
	var lgtMatrix  = m.identity(m.create());
	var dvMatrix   = m.identity(m.create());
	var dpMatrix   = m.identity(m.create());
  var dvpMatrix  = m.identity(m.create());
  
  const light_position = [0, 1, 0];
  const light_up_direction = [0, 0, -1];
  
  let count = 0;

  gl.enable(gl.DEPTH_TEST);
  gl.depthFunc(gl.LEQUAL);
  gl.enable(gl.CULL_FACE);

  const f_buffer_width  = gl.canvas.width;
  const f_buffer_height = gl.canvas.height;
  const f_buffer = create_framebuffer(gl, f_buffer_width, f_buffer_height);

  const tick = () => {
    requestAnimationFrame(tick);

    const eye_position = [];
    const cam_up_direction = [];

    q.toVecIII([0, 70, 0], qt, eye_position);
    q.toVecIII([0, 0, -1], qt, cam_up_direction);
    m.lookAt(eye_position, [0, 0, 0], cam_up_direction, vMatrix);
    m.perspective(45, gl.canvas.width / gl.canvas.height, 0.1, 150, pMatrix);
    m.multiply(pMatrix, vMatrix, tmpMatrix);

    m.identity(tMatrix);
    tMatrix[0]  = 0.5, tMatrix[1]  = 0.0, tMatrix[2]  = 0.0, tMatrix[3]  = 0.0,
    tMatrix[4]  = 0.0, tMatrix[5]  = 0.5, tMatrix[6]  = 0.0, tMatrix[7]  = 0.0,
    tMatrix[8]  = 0.0, tMatrix[9]  = 0.0, tMatrix[10] = 1.0, tMatrix[11] = 0.0,
    tMatrix[12] = 0.5, tMatrix[13] = 0.5, tMatrix[14] = 0.0, tMatrix[15] = 1.0;

    const r = e_range.value;
    light_position[0] = 0.0 * r;
    light_position[1] = 1.0 * r;
    light_position[2] = 0.0 * r;

    m.lookAt(light_position, [0, 0, 0], light_up_direction, dvMatrix);
    m.perspective(90, 1.0, 0.1, 150, dpMatrix);
    m.multiply(tMatrix, dpMatrix, dvpMatrix);
    m.multiply(dvpMatrix, dvMatrix, tMatrix);
    m.multiply(dpMatrix, dvMatrix, dvpMatrix);

    const t = e_radio.checked;

    gl.useProgram(d_prg);
    gl.bindFramebuffer(gl.FRAMEBUFFER, f_buffer.frame);
    gl.clearColor(1, 1, 1, 1);
    gl.clearDepth(1.0);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    set_attribute(gl, dt_vbo_list, d_atts);
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, t_index);
    
    const rad = ((count + 36) % 360) * Math.PI / 180;
    const rad2 = 0;
    const ifl = 1;

    m.identity(mMatrix);
    m.rotate(mMatrix, rad2, [0, 1, 0], mMatrix);
    m.translate(mMatrix, [0, ifl * 10 + 10, (ifl - 2) * 7], mMatrix);
    m.rotate(mMatrix, rad, [1, 1, 0], mMatrix);
    m.multiply(dvpMatrix, mMatrix, lgtMatrix);
    gl.uniformMatrix4fv(d_uni_location[0], false, lgtMatrix);
    gl.uniform1i(d_uni_location[1], t);
    gl.drawElements(gl.TRIANGLES, torus_data.index.length, gl.UNSIGNED_SHORT, 0);

    gl.useProgram(s_prg);
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, f_buffer.texture);

    gl.clearColor(0, 0.7, 0.7, 1.0);
    gl.clearDepth(1.0);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    set_attribute(gl, t_vbo_list, s_atts);
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, t_index);
    m.identity(mMatrix);
    m.rotate(mMatrix, rad2, [0, 1, 0], mMatrix);
    m.translate(mMatrix, [0, ifl * 10 + 10, (ifl - 2) * 7], mMatrix);
    m.rotate(mMatrix, rad, [1, 1, 0], mMatrix);
    m.multiply(tmpMatrix, mMatrix, mvpMatrix);
    m.inverse(mMatrix, invMatrix);
    m.multiply(dvpMatrix, mMatrix, lgtMatrix);
    gl.uniformMatrix4fv(s_uni_location[0], false, mMatrix);
    gl.uniformMatrix4fv(s_uni_location[1], false, mvpMatrix);
    gl.uniformMatrix4fv(s_uni_location[2], false, invMatrix);
    gl.uniformMatrix4fv(s_uni_location[3], false, tMatrix);
    gl.uniformMatrix4fv(s_uni_location[4], false, lgtMatrix);
    gl.uniform3fv(s_uni_location[5], light_position);
    gl.uniform1i(s_uni_location[6], 0);
    gl.uniform1i(s_uni_location[7], t);
    gl.drawElements(gl.TRIANGLES, torus_data.index.length, gl.UNSIGNED_SHORT, 0);

    set_attribute(gl, v_vbo_list, s_atts);
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, v_index);
    m.identity(mMatrix);
    m.translate(mMatrix, [0, -50, 0], mMatrix);
    m.scale(mMatrix, [50, 0, 50], mMatrix);
		m.multiply(tmpMatrix, mMatrix, mvpMatrix);
    m.inverse(mMatrix, invMatrix);
    m.multiply(dvpMatrix, mMatrix, lgtMatrix);
    gl.uniformMatrix4fv(s_uni_location[0], false, mMatrix);
    gl.uniformMatrix4fv(s_uni_location[1], false, mvpMatrix);
    gl.uniformMatrix4fv(s_uni_location[2], false, invMatrix);
    gl.uniformMatrix4fv(s_uni_location[3], false, tMatrix);
    gl.uniformMatrix4fv(s_uni_location[4], false, lgtMatrix);
    gl.drawElements(gl.TRIANGLES, index.length, gl.UNSIGNED_SHORT, 0);

    gl.flush();
  };

  tick();
});

function set_attribute(gl, vbo_list, loc_str_list) {
  for (let i = 0; i < vbo_list.length; i++) {
    const vbo = vbo_list[i];
    const { location, stride } = loc_str_list[i];
    gl.bindBuffer(gl.ARRAY_BUFFER, vbo);
    gl.enableVertexAttribArray(location);
    gl.vertexAttribPointer(location, stride, gl.FLOAT, false, 0, 0);
  }
}

function create_framebuffer(gl, width, height) {
  const buffer = gl.createFramebuffer();
  gl.bindFramebuffer(gl.FRAMEBUFFER, buffer);

  const depth_render_buffer = gl.createRenderbuffer();
  gl.bindRenderbuffer(gl.RENDERBUFFER, depth_render_buffer);
  gl.renderbufferStorage(gl.RENDERBUFFER, gl.DEPTH_COMPONENT16, width, height);
  gl.framebufferRenderbuffer(gl.FRAMEBUFFER, gl.DEPTH_ATTACHMENT, gl.RENDERBUFFER, depth_render_buffer);

  const f_texture = gl.createTexture();
  gl.bindTexture(gl.TEXTURE_2D, f_texture);

  gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, width, height, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);

  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);

  gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, f_texture, 0);

  gl.bindTexture(gl.TEXTURE_2D, null);
  gl.bindRenderbuffer(gl.RENDERBUFFER, null);
  gl.bindFramebuffer(gl.FRAMEBUFFER, null);

  return { frame: buffer, render: depth_render_buffer, texture: f_texture };
}

function create_ibo(gl, index) {
  const ibo = gl.createBuffer();
  gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, ibo);
  gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, new Int16Array(index), gl.STATIC_DRAW);
  gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, null);
  return ibo;
}

function create_vbo(gl, data) {
  const vbo = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, vbo);
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(data), gl.STATIC_DRAW);
  gl.bindBuffer(gl.ARRAY_BUFFER, null);
  return vbo;
}

function create_program(gl, vs, fs) {
  const prg = gl.createProgram();
  [vs, fs].forEach(s => gl.attachShader(prg, s));
  gl.linkProgram(prg);
  return prg;
}

function create_shader(gl, id) {
  const source = document.getElementById(id).textContent;
  const shader = gl.createShader(/vs/.test(id) ? gl.VERTEX_SHADER : gl.FRAGMENT_SHADER);
  gl.shaderSource(shader, source);
  gl.compileShader(shader);
  return shader;
}
