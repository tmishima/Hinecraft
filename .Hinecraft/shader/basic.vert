#version 130

in vec4 VertexPosition;
in vec4 VertexColor;

out vec4 Color;

uniform mat4 RotationMatrix;

void main () {
  Color = VertexColor;
  //gl_Position = RotationMatrix * VertexPosition;
  gl_Position = VertexPosition;
  //gl_Position = ftransform();
}

