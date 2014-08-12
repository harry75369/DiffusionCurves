#!/usr/bin/env python2

import sys
from PIL import Image
from PIL import ImageColor
from PIL import ImageDraw
import json

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

  def __init__(self, width, height):
    self.width = width
    self.height = height
    self.img = Image.new("RGB", (width, height), ImageColor.getrgb("#fff"))
    self.imgdraw = ImageDraw.Draw(self.img)

  def show(self):
    self.img.transpose(Image.FLIP_TOP_BOTTOM).show()

  def save(self, filename):
    self.img.transpose(Image.FLIP_TOP_BOTTOM).save(filename)

  def draw(self, curves):
    for c in curves.curves:
      green = ImageColor.getrgb("#0f0")
      red = ImageColor.getrgb("#f00")
      #self.imgdraw.line(c.get_control_points(self.width, self.height), green)
      self.imgdraw.line(c.get_curve_points(self.width, self.height, 500), red)


if __name__ == "__main__":
  if len(sys.argv) != 2:
    print "Usage: main.py file.dvg"
    sys.exit(0)

  curves = DiffusionCurves(sys.argv[1])
  canvas = Canvas(400, 300)
  canvas.draw(curves)
  canvas.show()
  canvas.save(sys.argv[1]+".png")

