#version 130

in vec4 Color;
in vec2 TexCoord;
in vec3 Position;
in vec3 Normal;
in vec4 ShadowCoord;

out vec4 FragColor;

uniform int TexEnbFlg;
uniform int ColorBlandType;
uniform sampler2D TexUnit;

// for Fog
uniform float FogMaxDist = 700.0;
uniform float FogMinDist = 10.0;
uniform vec3 FogColor = vec3 (1.0,1.0,1.0);

uniform int ShadowSW = 0;
uniform int LightMode = 0;

uniform sampler2D ShadowMap;
uniform vec3 GlobalLightVec = vec3 (0.0,1.0,0.0);

void RenderPass ()
{
  vec4 ShadarColor;
  float dist = length (Position.xz);
  float fogFactor = (FogMaxDist - dist) / (FogMaxDist - FogMinDist);
  fogFactor = clamp ( fogFactor, 0.0, 1.0);
  vec4 txc = texture (TexUnit, TexCoord);
  vec4 tColor;
  vec4 shadowCoordinateWdivide = ShadowCoord / ShadowCoord.w ;
  float diffuse  = clamp(dot(Normal, GlobalLightVec), 0.4, 0.8);
  vec3 diffAndSpec = Color.rgb * vec3(diffuse); 
  float distanceFromLight = texture (ShadowMap,shadowCoordinateWdivide.st).z;
  float shadow = 1.0;
		
  // Used to lower moire pattern and self-shadowing
  shadowCoordinateWdivide.z += 0.00005;

  if (LightMode == 0)  
  {
    tColor = Color; 
  }
  else
  {
    if (ShadowCoord.w > 0.0)
      shadow = distanceFromLight < shadowCoordinateWdivide.z ? 0.5 : 1.0 ;
    tColor = vec4(diffAndSpec * shadow , 1.0);
  }

  if (TexEnbFlg > 0)
  {
     if (ColorBlandType == 1) 
     {
       ShadarColor = mix ( tColor, txc, 0.8);
     }
     else {
       ShadarColor = tColor * txc;
     }
  }
  else
  { 
    ShadarColor = tColor;
  }
  FragColor = mix ( vec4 (FogColor,1.0), ShadarColor, fogFactor);
}

void main(){
  if ( ShadowSW == 0 ) {
     RenderPass ();
  }
}


