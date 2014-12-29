% Prefilter images
if exist('cache.mat', 'file')
  load('cache.mat', 'imgs', 'n');
else
  img = double(imread('fluid.png'))/255;
  n = 4;
  imgs = hierarchical(img, n);
  save('cache.mat', 'imgs', 'n');
end

% Display images
for i = n:n
  img = imgs(:,:,i);
  figure;imshow(img,[0 100])
end

% Find candidate curve pixels
g = fspecial('gaussian', 5);
[gx, gy] = gradient(g);
[gxx, gxy] = gradient(gx);
[gyx, gyy] = gradient(gy);
for i = n:n
  laplacian = abs(del2(imgs(:,:,i)));
  figure;imshow(laplacian,[min(laplacian(:)) max(laplacian(:))])
  dxx = conv2(laplacian, gxx, 'same');
  dxy = conv2(laplacian, gxy, 'same');
  dyy = conv2(laplacian, gyy, 'same');
  nx = zeros(size(laplacian));
  ny = zeros(size(laplacian));
  for x = 1:size(laplacian, 1)
    for y = 1:size(laplacian, 2)
      h = [dxx(x,y) dxy(x,y); dxy(x,y) dyy(x,y)];
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
  n = zeros(size(laplacian,1), size(laplacian,2), 3);
  n(:,:,1) = 255*(1-ny)/2;
  n(:,:,2) = 255*(1+nx)/2;
  figure;imshow(n);
end
  
  