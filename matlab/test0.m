% This test shows the prefiltered image stack
img = double(imread('gf.jpg'))/255;
n = 10;
imgs = hierarchical(img, n);

% Display filtered images
for i = 1:n
  img = imgs(:,:,i);
  figure;imshow(img,[0 100])
end