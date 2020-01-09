export declare function context(id: string): WebGLRenderingContext;
export declare function program(gl: WebGLRenderingContext, vss: string, fss: string): WebGLProgram;
export declare function uniform(gl: WebGLRenderingContext, prg: WebGLProgram, type: string, name: string, value: any): void;
export declare function attribute(gl: WebGLRenderingContext, prg: WebGLProgram, vbo: WebGLBuffer, name: string, stride: number): void;
export declare function buffer(gl: WebGLRenderingContext, type: number, value: any): WebGLBuffer;

export declare const VERTEX_SOURCE: string;
export declare const FRAGMENT_SOURCE: string;
