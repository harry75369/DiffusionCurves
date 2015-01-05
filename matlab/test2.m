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
  g = fspecial('gaussian', 11);
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
figure;imshow(n)