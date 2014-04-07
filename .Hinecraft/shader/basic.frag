#version 130

in vec4 Color;
in vec2 TexCoord;

out vec4 FragColor;

uniform int TexEnbFlg;
uniform int ColorBlandType;
uniform sampler2D TexUnit;

// for Fog
in vec3 Position;

uniform float FogMaxDist = 700.0;
uniform float FogMinDist = 10.0;
uniform vec3 FogColor = vec3 (1.0,1.0,1.0);

void main(){
  vec4 ShadarColor;
  float dist = length (Position.xz);
  float fogFactor = (FogMaxDist - dist) / (FogMaxDist - FogMinDist);
  fogFactor = clamp ( fogFactor, 0.0, 1.0);

  vec4 txc = texture (TexUnit, TexCoord);
  if (TexEnbFlg > 0)
  {
     if (ColorBlandType == 1) 
     {
       ShadarColor = mix ( Color, txc, 0.8);
     }
     else {
       ShadarColor = Color * txc;
     }
  }
  else
  {
    ShadarColor = Color;
  }

  FragColor = mix ( vec4 (FogColor,1.0), ShadarColor, fogFactor);
}

 
