#version 130

in vec4 VertexPosition;
in vec4 VertexColor;

out vec4 Color;

void main () {
  Color = VertexColor;
  //Color = vec3(0.0,0.0,1.0);
  //gl_Position = vec4(VertexPosition, 1.0);
  gl_Position = VertexPosition;
  //gl_Position = ftransform();
}

