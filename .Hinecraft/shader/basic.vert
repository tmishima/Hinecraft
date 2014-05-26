#version 130

in vec3 VertexPosition;
in vec4 VertexColor;
in vec2 VertexTexture;
in vec3 VertexNormal;

out vec4 Color;
out vec2 TexCoord;
out vec3 Position;
out vec3 Normal;
out vec4 ShadowCoord;

uniform mat4 mMatrix;
uniform mat4 mvpMatrix;
uniform mat4 tMatrix;

void main() {
  Color = VertexColor;
  TexCoord = VertexTexture;

  Position    = (mMatrix * vec4(VertexPosition, 1.0)).xyz;
  Normal      = VertexNormal;
  ShadowCoord = tMatrix * vec4(VertexPosition, 1.0);
  gl_Position = mvpMatrix * vec4(VertexPosition, 1.0);
}

