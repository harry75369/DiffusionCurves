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

    def img2mat(img):
      mat = np.zeros(img.size)
      w, h = img.size
      for i in range(w):
        for j in range(h):
          mat[i][j] = img.getpixel((i,j)) / 255.0
      return mat

    def mat2img(mat):
      img = Image.new("L", mat.shape, 0)
      w, h = mat.shape
      for i in range(w):
        for j in range(h):
          img.putpixel((i,j), max(0, min(255, 255*mat[i][j])))
      return img

    def ddx(mat):
      ret = np.zeros(mat.shape)
      w, h = mat.shape
      for j in range(h):
        ret[0][j] = mat[1][j] - mat[0][j]
        for i in range(1,w-1):
          ret[i][j] = (mat[i+1][j]-mat[i-1][j])/2
        ret[w-1][j] = mat[w-1][j] - mat[w-2][j]
      return ret

    def ddy(mat):
      ret = np.zeros(mat.shape)
      w, h = mat.shape
      for i in range(w):
        ret[i][0] = mat[i][1] - mat[i][0]
        for j in range(1,h-1):
          ret[i][j] = (mat[i][j+1]-mat[i][j-1])/2
        ret[i][h-1] = mat[i][h-1] - mat[i][h-2]
      return ret

    rgb = [Image.new("L", (self.width, self.height), 0),
           Image.new("L", (self.width, self.height), 0),
           Image.new("L", (self.width, self.height), 0)]
    for c in curves.curves: # init boundary condition
      curve_points = c.get_curve_points(self.width, self.height, 40)
      lcolors = c.get_colors("L", curve_points)
      rcolors = c.get_colors("R", curve_points)
      for i in range(3):
        Canvas.interpolate_colored_curve(rgb[i], curve_points, lcolors[i], False)
        Canvas.interpolate_colored_curve(rgb[i], curve_points, rcolors[i], True)
    n = self.width * self.height
    index = lambda x, y: x*self.height+y
    inrange = lambda x, y: x>=0 and x<self.width and y>=0 and y<self.height
    dirs = [(-1,0),(1,0),(0,-1),(0,1)]
    for c in range(3): # solve poisson equation in each component
      print "doing component %d" % c
      mat = img2mat(rgb[c])
      nabla_x = ddx(mat)
      nabla_y = ddy(mat)
      laplace_x = ddx(nabla_x)
      laplace_y = ddy(nabla_y)
      A = np.zeros((n, n))
      b = np.zeros((n, 1))
      for i in range(self.width):
        for j in range(self.height):
          idx = index(i, j)
          b[idx] = laplace_x[i][j] + laplace_y[i][j]
          if mat[i][j] < 1e-5: # if pixel(i,j) is unknown
            A[idx][idx] = -4
            for dx, dy in dirs:
              if inrange(i+dx, j+dy):
                if mat[i+dx][j+dy] < 1e-5: # if this pixel is not boundary
                  idx2 = index(i+dx, j+dy)
                  A[idx][idx2] = 1
                else:
                  b[idx] = b[idx] - mat[i+dx][j+dy]
          else:
            A[idx][idx] = 1
            b[idx] = mat[i][j]
      print "building done"
      x = np.linalg.solve(A, b)
      print "solving done"
      for i in range(self.width):
        for j in range(self.height):
          mat[i][j] = x[index(i,j)]
      rgb[c] = mat2img(mat)
    self.img = Image.merge("RGB", rgb)

  @classmethod
  def interpolate_colored_curve(cls, img, cps, color, offset):

    def bresenham(img, x1, y1, x2, y2, c1, c2, offset_flag):
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
      if dx > dy:
        offset = (0,-1) if x2>x1 else (0,1)
      else:
        offset = (1,0) if y2>y1 else (-1,0)
      if not offset_flag:
        offset = (0,0)
      length = int(max(dx, dy))
      dc = (c2-c1) / length
      for i in range(length+1):
        ix = int(x)+offset[0]
        iy = int(y)+offset[1]
        if ix >= 0 and ix < w and iy >= 0 and iy < h:
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
      bresenham(img, p0_x, p0_y, p1_x, p1_y, c0, c1, offset)

if __name__ == "__main__":
  if len(sys.argv) != 2:
    print "Usage: main.py file.dvg"
    sys.exit(0)

  curves = DiffusionCurves(sys.argv[1])
  #canvas = Canvas(180, 135)
  canvas = Canvas(100, 75)
  canvas.draw(curves)
  canvas.show()
  canvas.save(sys.argv[1]+".png")
