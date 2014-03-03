#version 130

in vec3 Color;

out vec4 FragColor;

void main () {
  FragColor = vec4(Color, 1.0);
//  gl_FragColor = vec4(0.0,0.0,1.0,1.0);
}
 
