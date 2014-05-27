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
uniform vec3 LightPosition = vec3 (0,400,0);
uniform float MaterialShininess = 0.5;

/*
vec3 phongModelDiffAndSpec()
{
    vec3 n = Normal;
    if( !gl_FrontFacing ) n = -n;
    vec3 s = normalize(vec3(LightPosition) - Position);
    vec3 v = normalize(-Position.xyz);
    vec3 r = reflect( -s, n );
    float sDotN = max( dot(s,n), 0.0 );
    vec3 diffuse = Color.rgb * sDotN; // Light.Intensity * Material.Kd * sDotN;
    vec3 spec = vec3(0.0);
    if( sDotN > 0.0 )
        spec =  Color.rbg *    // Light.Intensity * Material.Ks *
            pow( max( dot(r,v), 0.0 ), MaterialShininess );

    return clamp (diffuse + spec, vec3(0.0,0.0,0.0), vec3(0.8,0.8,0.8));
}
*/
uniform vec3 lightDirection = vec3 (0.0,1.0,0.0);

void RenderPass ()
{
  vec4 ShadarColor;
  float dist = length (Position.xz);
  float fogFactor = (FogMaxDist - dist) / (FogMaxDist - FogMinDist);
  fogFactor = clamp ( fogFactor, 0.0, 1.0);
  vec4 txc = texture (TexUnit, TexCoord);
  vec4 tColor;
  vec4 shadowCoordinateWdivide = ShadowCoord / ShadowCoord.w ;
  vec3 ambient = vec3 (0.1,0.1,0.1);
  //vec3 diffAndSpec = phongModelDiffAndSpec();
  vec3  invLight = lightDirection;
  float diffuse  = clamp(dot(Normal, invLight), 0.1, 1.0);
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
      shadow = distanceFromLight < shadowCoordinateWdivide.z ? 0.1 : 1.0 ;
    tColor = vec4(diffAndSpec * shadow + ambient, 1.0);
    //tColor =  vec4((0.8,0.8,0.8) * shadow + ambient, 1.0);
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
  //FragColor = mix ( vec4 (FogColor,1.0), tColor, fogFactor);
  //if (ShadowCoord.w > 0.0)
  //  shadow = distanceFromLight < shadowCoordinateWdivide.z ? 0.1 : 1.0 ;
  //FragColor = vec4((0.8,0.8,0.8) * shadow + ambient, 1.0);
}

void main(){
  if ( ShadowSW == 0 ) {
     RenderPass ();
  }
}


