#version 130

in vec3 VertexPosition;
in vec4 VertexColor;
in vec2 VertexTexture;
in vec3 VertexNormal;

out vec4 Color;
out vec2 TexCoord;
out vec3 Position;

uniform mat3 SclMat;
uniform mat4 RotMat;
uniform mat4 ProjViewMat; //projection_matrix modelview_matrix

uniform vec4 ambientColor = vec4 (0.2,0.2,0.2,0.2);
uniform vec3 lightDirection = vec3 (0,1,0);

uniform int LightMode;

void main() {
  vec4 vertPos;
  if (LightMode == 0)
  {
    Color = VertexColor;
  }
  else
  {
    mat3 rot = mat3 (RotMat);
    vec3 invLight = lightDirection; // normalize (lightDirection);
    float diffuse = clamp (dot (VertexNormal, invLight), 0.0, 1.0);

    Color = clamp ( VertexColor * vec4( vec3(diffuse), 1.0)
                    + ambientColor, 0.0, 1.0);  
  }
  TexCoord = VertexTexture;
  vertPos = ProjViewMat * RotMat * vec4(SclMat * VertexPosition,1.0);
  Position = vertPos.xyz;
  gl_Position = vertPos;
}


