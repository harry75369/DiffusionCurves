img = double(imread('gf.jpg'))/255;
n = 4;
imgs = hierarchical(img, n);
for i = 1:n
  figure;
  img = imgs(:,:,i);
  imshow(img,[0 100])
end