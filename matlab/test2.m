% Given results of test1 (no major difference between two methods when
% window size >= 9), this test use the discrete second partial derivatives
% to calculate the normals.
%
% According to the observation, although the derivatives are similar, the
% Gaussian kernel method can give better normals.

img = double(imread('circle.png'))/255;
img = img(:,:,1);
lap = abs(del2(img));

% Calculate discrete second partial derivatives
if 1
  g = fspecial('gaussian', 9);
  [gx, gy] = gradient(g);
  [gxx, gxy] = gradient(gx);
  [gyx, gyy] = gradient(gy);
  dxx = conv2(lap, gxx, 'same');
  dxy = conv2(lap, gxy, 'same');
  dyx = conv2(lap, gyx, 'same');
  dyy = conv2(lap, gyy, 'same');
else
  [dx, dy] = gradient(lap);
  [dxx, dxy] = gradient(dx);
  [dyx, dyy] = gradient(dy);
end

% Calculate normals
nx = zeros(size(img));
ny = zeros(size(img));
for i = 1:size(img,1)
  for j = 1:size(img,2)
    h = [dxx(i,j) dxy(i,j);dyx(i,j) dyy(i,j)];
    [vec, val] = eig(h);
    if val(1,1) > val(2,2)
      nx(i,j) = vec(1,1);
      ny(i,j) = vec(2,1);
    else
      nx(i,j) = vec(1,2);
      ny(i,j) = vec(2,2);
    end
  end
end
n = zeros(size(img,1), size(img,2), 3);
n(:,:,1) = (1-ny)/2;
n(:,:,2) = (1+nx)/2;
figure;imshow(lap)
figure;imshow(n)
e = edge(lap, 'canny');
figure;imshow(e)

% Calculate select points
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
     
    if laplacian(x,y) < 0.01
      continue
    end
     
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