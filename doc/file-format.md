# Description for File Format of Diffusion Vector Graphics

## Introduction

Diffusion Vector Graphics is a new kind of vector graphics that has native 
support for arbitray gradual color change effect, which can be seen as a new 
primitive added to the traditional vector graphics standard, namely the 
Scalable Vector Graphics (SVG).

This new primitive is called the Diffusion Curve. In this document, we will 
describe how diffusion curve is represented in a simple text format as a 
prerequisite for rendering.

Note for now this is just an over simplified format for vector graphics 
containing **only** diffusion curves. Further improvements is not likely in the
future.

## Revision History

 * 2014/07/11: Initial version. Chaoya Li.

## Diffusion Curve

A diffusion curve is a bezier curve along with additional color and blur 
attributes.

### Definition

### File Name Extension

Extension is dvg.

### File Content

```
[
  {
    "id": number,
    "control": [(x,y),(x,y)...],
    "lcolor": [(r,g,b,t),...],
    "rcolor": [(r,g,b,t),...],
    "blur": [(s,t),...]
  },
  ...
]
```
where number is unsigned integer, and x, y, r, g, b, t are floating points.  
(x,y) describes a control point position. (r,g,b,t) describes the color at 
parameter t. (s,t) describes the blurness at parameter t.

x, y are in range (-inf, inf). r, g, b, t, s are in range [0.0, 1.0].

There should be at least two control points for a curve. The default color is 
black and the default blurness is zero if not provided.

### Sample

```
$ cat sample.dvg

[
  {
    "id": 0,
    "control": [(0,0),(0.25,3),(0.75,-2),(1,1)],
    "lcolor": [(1.0,0.0,0.0,0.0),(0.0,1.0,0.0,0.5),(0.0,0.0,1.0,1.0)],
    "rcolor": [(0.0,1.0,1.0,0.0),(1.0,0.0,1.0,0.5),(1.0,1.0,0.0,1.0)],
    "blur": [(0,0),(0,5,0.5),(1,1)]
  }
]
```
## References

(to be filled in)
