% This test shows two methods to calculate the discrete second partial
% derivatives and their difference.
%
% According to the observation, direct double gradient compute is same with
% the ground truth, while the Gaussian kernel convole has minor difference
% with Fxy, but major difference with Fxx and Fyy when window size < 9, but
% little difference when window size >= 9.

% Ground truth
ds = 0.1;
[X,Y] = meshgrid(-5:ds:5,-5:ds:5);
F = exp(-(X.^2+Y.^2));%figure;surf(F)
%Fx = (-2*X).*exp(-(X.^2+Y.^2));figure;surf(Fx)
%Fy = (-2*Y).*exp(-(X.^2+Y.^2));figure;surf(Fy)
%Fxx = (4*X.^2-2).*exp(-(X.^2+Y.^2));figure;surf(Fxx)
%Fxy = (4*(X.*Y)).*exp(-(X.^2+Y.^2));figure;surf(Fxy)
Fyy = (4*Y.^2-2).*exp(-(X.^2+Y.^2));figure;surf(Fyy)

% Direct double gradient compute
[fx, fy] = gradient(F, ds, ds);
[fxx, fxy] = gradient(fx, ds, ds);
[fyx, fyy] = gradient(fy, ds, ds);
%figure;surf(fx)
%figure;surf(fy)
%figure;surf(fxx)
%figure;surf(fxy)
figure;surf(fyy)

% Convole with Gaussian kernel
g = fspecial('gaussian', 11);
[gx, gy] = gradient(g, ds, ds);
[gxx, gxy] = gradient(gx, ds, ds);
[gyx, gyy] = gradient(gy, ds, ds);
%Gx = conv2(F, gx, 'same');figure;surf(Gx)
%Gy = conv2(F, gy, 'same');figure;surf(Gy)
%Gxx = conv2(F, gxx, 'same');figure;surf(Gxx)
%Gxy = conv2(F, gxy, 'same');figure;surf(Gxy)
Gyy = conv2(F, gyy, 'same');figure;surf(Gyy)
