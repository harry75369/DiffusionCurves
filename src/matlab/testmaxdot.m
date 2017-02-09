function [xx, yy, maxdot] = testmaxdot(xx0, yy0, maxdot0, normal, selected, nx, ny, x, y, size)
  if x > 1 && x <= size(1) && y > 1 && y <= size(2) && selected(x,y) == 1
    nt = [-ny(x,y) nx(x,y)];
    v = nt*normal;
    if v > maxdot0 && v > 0.7071
      maxdot = v;
      xx = x;
      yy = y;
      return
    end
  end
  xx = xx0;
  yy = yy0;
  maxdot = maxdot0;
end