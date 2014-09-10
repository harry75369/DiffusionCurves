# File Format Documentation

## Revision History

 * 2014/07/11: Initial version. Chaoya Li.
 * 2014/09/10: Initialized descriptions of Vector Graphics Complex. Chaoya Li.

## Brief

This document describes the file format of our vector graphics.

It currently contains two parts, namely the diffusion curves and the vector 
graphics complex. For the time being these two parts are two separate file 
formats and are planned to be merged in the future.

## Diffusion Curves

Diffusion Curves is a new kind of primitive in vector graphics that adds native 
support for arbitray gradual (smooth) color change effect. Technically, a 
diffusion curve is just a bezier curve along with additional color and blur 
attributes.

Note: for now this is just an over simplified format for vector graphics 
containing **only** diffusion curves.

### File Name Extension

Extension is dvg.

### File Content

```
[
  {
    "id": number,
    "control": "[(x,y),(x,y)...]",
    "lcolor": "[(r,g,b,t),...]",
    "rcolor": "[(r,g,b,t),...]",
    "blur": "[(s,t),...]"
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
    "control": "[(0,0),(0.25,3),(0.75,-2),(1,1)]",
    "lcolor": "[(1.0,0.0,0.0,0.0),(0.0,1.0,0.0,0.5),(0.0,0.0,1.0,1.0)]",
    "rcolor": "[(0.0,1.0,1.0,0.0),(1.0,0.0,1.0,0.5),(1.0,1.0,0.0,1.0)]",
    "blur": "[(0,0),(0,5,0.5),(1,1)]"
  }
]
```

## Vector Graphics Complex

 (To be filled in)
