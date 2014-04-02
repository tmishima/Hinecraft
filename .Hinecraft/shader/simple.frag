#version 130

in vec4 Color;
in vec2 TexCoord;

out vec4 FragColor;

uniform int TexEnbFlg;
uniform sampler2D TexUnit;

void main(){
  if (TexEnbFlg > 0)
  {
     FragColor = Color * (texture (TexUnit, TexCoord));
  }
  else
  {
     FragColor = Color;
  }
}

 
