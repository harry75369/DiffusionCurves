#!/usr/bin/env python2

import sys
from PIL import Image
from PIL import ImageColor
import json
import numpy as np

class DiffusionCurve:

  def __init__(self, id, control, lcolor, rcolor, blur):
    self.id = id
    self.control = control
    self.lcolor = lcolor
    self.rcolor = rcolor
    self.blur = blur

  def get_control_points(self, w, h):
    cps = []
    for cp in self.control:
      cps.append((w*cp[0], h*cp[1]))
    return cps

  def get_curve_points(self, w, h, subdiv):
    """
    de Casteljau algorithm, complexity O(subdiv * n^2)
    """

    def reduction(p, n, t):
      for k in range(n-1):
        pk0 = (1-t)*p[k][0]+t*p[k+1][0]
        pk1 = (1-t)*p[k][1]+t*p[k+1][1]
        p[k] = (pk0, pk1)
      return p

    cps = self.get_control_points(w, h)
    n = len(cps)
    cups = []
    dt = 1.0 / subdiv
    t = 0
    for i in range(subdiv+1):
      p = cps[:]
      for j in range(n-1):
        p = reduction(p, n-j, t)
      cups.append(p[0])
      t = t + dt
    return cups

  def get_colors(self, direction, cps):
    """
    Input: direction and curve points
    Output: each color component (RGB) at these points to that direction
    """
    choice = {"L": self.lcolor, "R": self.rcolor}
    source = choice[direction]
    colors = []
    n = len(cps)
    for i in range(3):
      c = np.zeros((n,1))
      for j in range(len(source)):
        idx = int(n*source[j][3]-0.5)
        c[idx] = source[j][i]
      colors.append(c)
    for i in range(3): # interpolate
      nonzeropos = np.nonzero(colors[i])[0]
      if nonzeropos[0] != 0:
        nonzeropos = np.append(0, nonzeropos)
      if nonzeropos[-1] != n-1:
        nonzeropos = np.append(nonzeropos, n-1)
      for j in range(len(nonzeropos)-1):
        start = nonzeropos[j]
        end = nonzeropos[j+1]
        start_color = colors[i][start]
        end_color = colors[i][end]
        dc = (end_color - start_color) / (end-start)
        for k in range(start, end+1):
          colors[i][k] = (k-start) * dc + start_color
    return colors

class DiffusionCurves:

  def __init__(self, filename):
    self.curves = []
    for c in json.load(open(filename)):
      curve = DiffusionCurve(c['id'],
                             eval(c['control']),
                             eval(c['lcolor']),
                             eval(c['rcolor']),
                             eval(c['blur']))
      self.curves.append(curve)

class Canvas:

  white = ImageColor.getrgb("#fff")
  red = ImageColor.getrgb("#f00")
  green = ImageColor.getrgb("#0f0")
  blur = ImageColor.getrgb("#00f")
  black = ImageColor.getrgb("#000")

  def __init__(self, width, height):
    self.width = width
    self.height = height
    self.img = Image.new("RGB", (width, height), Canvas.white)

  def show(self):
    self.img.transpose(Image.FLIP_TOP_BOTTOM).show()

  def save(self, filename):
    self.img.transpose(Image.FLIP_TOP_BOTTOM).save(filename)

  def draw(self, curves):
    #for c in curves.curves:
      #self.imgdraw.line(c.get_control_points(self.width, self.height), Canvas.green)
      #self.imgdraw.line(c.get_curve_points(self.width, self.height, 500), Canvas.red)
    r = Image.new("L", (self.width, self.height), 255)
    g = Image.new("L", (self.width, self.height), 255)
    b = Image.new("L", (self.width, self.height), 255)
    for c in curves.curves:
      curve_points = c.get_curve_points(self.width, self.height, 300)
      colors = c.get_colors("L", curve_points)
      Canvas.interpolate_colored_curve(r, curve_points, colors[0])
      Canvas.interpolate_colored_curve(g, curve_points, colors[1])
      Canvas.interpolate_colored_curve(b, curve_points, colors[2])
    self.img = Image.merge("RGB", [r,g,b])

  @classmethod
  def interpolate_colored_curve(cls, img, cps, color):

    def bresenham(img, x1, y1, x2, y2, c1, c2):
      w = img.size[0]
      h = img.size[1]
      x = x1
      y = y1
      dx = np.abs(x2-x1)
      dy = np.abs(y2-y1)
      sx = 0 if dx < 1e-8 else (x2-x1)/dx
      sy = 0 if dy < 1e-8 else (y2-y1)/dy
      e = 0
      if dx > dy:
        e = 2 * dy - dx
      else:
        e = 2 * dx - dy
      length = int(max(dx, dy))
      dc = (c2-c1) / length
      for i in range(length+1):
        ix = max(0, min(w-1, int(x)))
        iy = max(0, min(h-1, int(y)))
        img.putpixel((ix,iy), max(0, min(255, int((i*dc+c1)*255))))
        while e > 0:
          if dx > dy:
            y = y + sy
            e = e - 2*dx
          else:
            x = x + sx
            e = e - 2*dy
        if dx > dy:
          x = x + sx
          e = e + 2*dy
        else:
          y = y + sy
          e = e + 2*dx

    n = len(cps)
    for i in range(n-1):
      p0_x = cps[i][0]
      p0_y = cps[i][1]
      p1_x = cps[i+1][0]
      p1_y = cps[i+1][1]
      c0 = color[i]
      c1 = color[i+1]
      bresenham(img, p0_x, p0_y, p1_x, p1_y, c0, c1)

if __name__ == "__main__":
  if len(sys.argv) != 2:
    print "Usage: main.py file.dvg"
    sys.exit(0)

  curves = DiffusionCurves(sys.argv[1])
  canvas = Canvas(400, 300)
  canvas.draw(curves)
  canvas.show()
  canvas.save(sys.argv[1]+".png")

