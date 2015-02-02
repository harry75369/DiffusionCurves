#version 330 compatibility

layout(location = 0) in vec4 vPosition;
layout(location = 1) in vec3 vColor;

out vec3 fragmentColor;

void main() {
  gl_Position = gl_ModelViewProjectionMatrix * vPosition;
  fragmentColor = vColor;
}
