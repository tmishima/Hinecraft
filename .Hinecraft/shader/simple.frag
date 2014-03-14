#version 130

in vec4 Color;
in vec2 TexCoord;

out vec4 FragColor;

uniform sampler2D Boxtex;

void main(){
  FragColor = Color * (texture (Boxtex, TexCoord));
}

 
