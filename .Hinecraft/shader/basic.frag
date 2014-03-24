#version 130

in vec4 Color;
in vec2 TexCoord;

out vec4 FragColor;

uniform int TexEnbFlg;
uniform int ColorBlandType;
uniform sampler2D TexUnit;

void main(){
  vec4 txc = texture (TexUnit, TexCoord);
  if (TexEnbFlg > 0)
  {
     if (ColorBlandType == 1) 
     {
       FragColor = (Color * 0.8) + txc;
     }
     else {
       FragColor = Color * txc;
     }
  }
  else
  {
     FragColor = Color;
  }
}

 
