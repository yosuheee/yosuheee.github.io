precision mediump float;

uniform float time;
uniform vec2 mouse;
uniform vec2 resolution;

void main(void) {
    vec2 p = (gl_FragCoord.xy * 2.0 - resolution) / min(resolution.x, resolution.y);
    
    vec3 cPos = vec3(0.0,  0.0,  3.0);
    vec3 cDir = vec3(0.0,  0.0, -1.0);
    vec3 cUp  = vec3(0.0,  1.0,  0.0);
    vec3 cSide = cross(cDir, cUp);
    float targetDepth = 1.0;

    vec3 ray = normalize(cSide * p.x + cUp * p.y + cDir * targetDepth * (sin(time) + 2.0) / 2.0);

    gl_FragColor = vec4(ray.xy, -ray.z, 1.0);
}
