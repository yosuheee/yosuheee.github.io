import { V3, M4 } from "./math/geometry/index.js";
import { uniform, buffer, attribute } from "./webgl.js";

export class Model {
  constructor({
    primitives = null,
    model      = null,
    gl         = null,
    type       = null,
    mvp_matrix = M4(),
    m_matrix   = M4(),
  }) {
    if (type == null) {
      throw new Error("must exist type");
    }
    if ((primitives == null || gl == null) && model == null) {
      throw new Error("must exist primitives or model");
    }
    if ((primitives != null && gl != null) && model != null) {
      throw new Error("must not be both primitives and model");
    }
    if (primitives != null && gl != null) {
      const { position, color, index, normal } = primitives.primitive();

      this.position = buffer(gl, gl.ARRAY_BUFFER, new Float32Array(position));
      if (color  != null) this.color  = buffer(gl, gl.ARRAY_BUFFER, new Float32Array(color));
      if (normal != null) this.normal = buffer(gl, gl.ARRAY_BUFFER, new Float32Array(normal));

      this.index = buffer(gl, gl.ELEMENT_ARRAY_BUFFER, new Int16Array(index));
      this.length = index.length;
    }
    if (model != null) {
      this.position = model.position;
      this.color    = model.color;
      this.normal   = model.normal;
      this.index    = model.index;
      this.length   = model.length;
    }
    this.type       = type;
    this.mvp_matrix = mvp_matrix;
    this.m_matrix   = m_matrix;
  }
  draw(gl, prg, params = {}) {
    this.draw_by_params(gl, prg, params);
  }
  draw_by_params(gl, prg, {
    light = V3(1, 1, 1),
    pos_name = "position",
    col_name = "color",
    nor_name = "normal",
    mvp_mat_name = "mvp_matrix",
    inv_mat_name = "inv_matrix",
    light_name = "light",
  }) {
    const list = [];
    list.push([this.position, pos_name, 3]);
    if (this.color  != null && col_name) list.push([this.color,  col_name, 4]);
    if (this.normal != null && nor_name) list.push([this.normal, nor_name, 3]);

    list.forEach(([vbo, name, stride]) => attribute(gl, prg, vbo, name, stride));
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, this.index);

    mvp_mat_name && uniform(gl, prg, "mat4", mvp_mat_name, this.mvp_matrix.primitive());
    inv_mat_name && uniform(gl, prg, "mat4", inv_mat_name, this.m_matrix.invert().transpose().primitive());
    light_name && uniform(gl, prg, "vec3", light_name, light.normalize().primitive());

    gl.drawElements(this.type, this.length, gl.UNSIGNED_SHORT, 0);
  }
  scale(...args) {
    return this.merge(this.mvp_matrix.scale(...args), this.m_matrix.scale(...args));
  }
  rotate(...args) {
    return this.merge(this.mvp_matrix.rotate(...args), this.m_matrix.rotate(...args));
  }
  translate(...args) {
    return this.merge(this.mvp_matrix.translate(...args), this.m_matrix.translate(...args));
  }
  lookAt(...args) {
    return this.merge(this.mvp_matrix.lookAt(...args));
  }
  ortho(...args) {
    return this.merge(this.mvp_matrix.ortho(...args));
  }
  perspective(...args) {
    return this.merge(this.mvp_matrix.perspective(...args));
  }
  merge(mvp_matrix, m_matrix = this.m_matrix) {
    return new Model({ model: this, type: this.type, mvp_matrix, m_matrix });
  }
}
