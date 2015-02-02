#version 330 compatibility

in  vec2 texCoord;

out vec3 color;

uniform sampler2D myTextureSampler;

void main() {
  color = texture(myTextureSampler, texCoord).rgb;
}
