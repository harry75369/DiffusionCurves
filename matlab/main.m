% Prefilter images
if exist('cache.mat', 'file')
  load('cache.mat', 'imgs', 'n');
else
  img = double(imread('fluid_middle.png'))/255;
  n = 5;
  imgs = hierarchical(img, n);
  save('cache.mat', 'imgs', 'n');
end

% Display filtered images
for i = n:n
  img = imgs(:,:,i);
  figure;imshow(img,[0 100])
end

% Find curved in the filtered images
g = fspecial('gaussian', 9);
[gx, gy] = gradient(g);
[gxx, gxy] = gradient(gx);
[gyx, gyy] = gradient(gy);
for i = n:n
  % calculate laplacian of the image
  laplacian = abs(del2(imgs(:,:,i)));
  %[min(laplacian(:)) max(laplacian(:))]
  %figure;imshow(laplacian,[min(laplacian(:)) max(laplacian(:))])
  
  % calculte curve normals
  dxx = conv2(laplacian, gxx, 'same');
  dxy = conv2(laplacian, gxy, 'same');
  dyx = conv2(laplacian, gyx, 'same');
  dyy = conv2(laplacian, gyy, 'same');
  nx = zeros(size(laplacian));
  ny = zeros(size(laplacian));
  for x = 1:size(laplacian, 1)
    for y = 1:size(laplacian, 2)
      h = [dxx(x,y) dxy(x,y); dyx(x,y) dyy(x,y)];
      [vec, val] = eig(h);
      if val(1,1) > val(2,2)
        nx(x, y) = vec(1,1);
        ny(x, y) = vec(2,1);
      else
        nx(x, y) = vec(1,2);
        ny(x, y) = vec(2,2);
      end
    end
  end
  
  % display perpendicular normals
  n = zeros(size(laplacian,1), size(laplacian,2), 3);
  n(:,:,1) = (1-ny)/2;
  n(:,:,2) = (1+nx)/2;
  figure;imshow(n)
  
  % calculate laplacian edges
  e = edge(laplacian, 'canny');
  figure;imshow(e)
  
  % calculate candidate curve pixels
  selected = zeros(size(e));
  n1 = [cos(7*pi/8) ; sin(7*pi/8) ];
  n2 = [cos(pi/8)   ; sin(pi/8)   ];
  n3 = [cos(3*pi/8) ; sin(3*pi/8) ];
  n4 = [cos(13*pi/8); sin(13*pi/8)];
  for x = 1:size(e,1)
    for y = 1:size(e,2)
      if e(x, y) == 0
        continue
      end
     
      %if laplacian(x,y) < 0.00001
      %  continue
      %end
     
      nt = [nx(x,y) ny(x,y)];
      if nt * n1 <= 0 && nt * n2 > 0
        xx1 = x + 1;
      elseif nt * n2 < 0 && nt * n1 >= 0
        xx1 = x - 1;
      else
        xx1 = x;
      end
      if nt * n3 <= 0 && nt * n4 > 0
        yy1 = y -1;
      elseif nt * n4 < 0 && nt * n3 >= 0
        yy1 = y + 1;
      else
        yy1 = y;
      end
      xx2 = 2 * x - xx1;
      yy2 = 2 * y - yy1;
      if xx1 > 0 && xx1 <= size(e,1) && ...
         yy1 > 0 && yy1 <= size(e,2) && ...
         laplacian(x,y) < laplacian(xx1,yy1)
        continue
      end
      if xx2 > 0 && xx2 <= size(e,1) && ...
         yy2 > 0 && yy2 <= size(e,2) && ...
         laplacian(x,y) < laplacian(xx2,yy2)
        continue
      end
      selected(x, y) = 1;
    end
  end
  figure;imshow(selected)
  
  % calculated conncected curve pixels
  s = size(selected);
  connected = zeros(s);
  for x = 1:size(selected,1)
    for y = 1:size(selected,2)
      if selected(x, y) == 0
        continue
      end
      normal = [-ny(x,y); nx(x,y)];
      xx = x;
      yy = y;
      maxdot = 0;
      [xx, yy, maxdot] = testmaxdot(xx, yy, maxdot, normal, selected, nx, ny, x,y+1, s);
      [xx, yy, maxdot] = testmaxdot(xx, yy, maxdot, normal, selected, nx, ny, x,y-1, s);
      [xx, yy, maxdot] = testmaxdot(xx, yy, maxdot, normal, selected, nx, ny, x+1,y, s);
      [xx, yy, maxdot] = testmaxdot(xx, yy, maxdot, normal, selected, nx, ny, x+1,y+1, s);
      [xx, yy, maxdot] = testmaxdot(xx, yy, maxdot, normal, selected, nx, ny, x+1,y-1, s);
      [xx, yy, maxdot] = testmaxdot(xx, yy, maxdot, normal, selected, nx, ny, x-1,y, s);
      [xx, yy, maxdot] = testmaxdot(xx, yy, maxdot, normal, selected, nx, ny, x-1,y+1, s);
      [xx, yy, maxdot] = testmaxdot(xx, yy, maxdot, normal, selected, nx, ny, x-1,y-1, s);      
      if maxdot ~= 0
        connected(x, y) = 1;
        connected(xx,yy) = 1;
        selected(x,y) = 0;
        selected(xx,yy) = 0;
        continue
      end
      [xx, yy, maxdot] = testmaxdot(xx, yy, maxdot, normal, selected, nx, ny, x+2,y+2, s);
      [xx, yy, maxdot] = testmaxdot(xx, yy, maxdot, normal, selected, nx, ny, x+2,y+1, s);
      [xx, yy, maxdot] = testmaxdot(xx, yy, maxdot, normal, selected, nx, ny, x+2,y,   s);
      [xx, yy, maxdot] = testmaxdot(xx, yy, maxdot, normal, selected, nx, ny, x+2,y-1, s);
      [xx, yy, maxdot] = testmaxdot(xx, yy, maxdot, normal, selected, nx, ny, x+2,y-2, s);
      [xx, yy, maxdot] = testmaxdot(xx, yy, maxdot, normal, selected, nx, ny, x+1,y+2, s);
      [xx, yy, maxdot] = testmaxdot(xx, yy, maxdot, normal, selected, nx, ny, x+1,y-2, s);
      [xx, yy, maxdot] = testmaxdot(xx, yy, maxdot, normal, selected, nx, ny, x,y+2,   s);
      [xx, yy, maxdot] = testmaxdot(xx, yy, maxdot, normal, selected, nx, ny, x,y-2,   s);
      [xx, yy, maxdot] = testmaxdot(xx, yy, maxdot, normal, selected, nx, ny, x-1,y+2, s);
      [xx, yy, maxdot] = testmaxdot(xx, yy, maxdot, normal, selected, nx, ny, x-1,y-2, s);
      [xx, yy, maxdot] = testmaxdot(xx, yy, maxdot, normal, selected, nx, ny, x-2,y+2, s);
      [xx, yy, maxdot] = testmaxdot(xx, yy, maxdot, normal, selected, nx, ny, x-2,y+1, s);
      [xx, yy, maxdot] = testmaxdot(xx, yy, maxdot, normal, selected, nx, ny, x-2,y,   s);
      [xx, yy, maxdot] = testmaxdot(xx, yy, maxdot, normal, selected, nx, ny, x-2,y-1, s);
      [xx, yy, maxdot] = testmaxdot(xx, yy, maxdot, normal, selected, nx, ny, x-2,y-2, s);
      if maxdot ~= 0
        connected(x, y) = 1;
        connected(xx,yy) = 1;
        selected(x, y) = 0;
        selected(xx,yy) = 0;
        xxx = round((x+xx-1)/2);
        yyy = round((y+yy-1)/2);
        connected(xxx,yyy) = 1;
        selected(xxx,yyy) = 0;
        continue;
      end
    end
  end
  %figure;imshow(connected);
  %figure;imshow(selected);
end
