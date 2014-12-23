function B = hierarchical(A, nLevels)
%HIERARCHICAL Generate a stack of prefiltered lightness images.
%   Described in Sec. 4.1 of
%
%     G. Xie, X. Sun, X. Tong and D. Nowrouzezahrai. Hierarchical Diffusion
%     Curves for Accurate Automatic Image Vectorization. ACM Trans. Graph.
%     ACM SIGGRAPH Asia 2014.
%
%   Input image A has to be a double precision matrix of size NxMx3 in RGB
%   color space with each channel on the closed interval [0,1].
%
%   Output B is a list of nLevels 2D-matrices of size NxM. The value in
%   each matrix is the lightness (L channel) of CIE-Lab colorspace. The
%   first matrix B(:,:,1) is the finest, while the last matrix B(:,:,end)
%   is the coarsest.
%

% Verify that the input image exists and is valid.
if ~exist('A', 'var') || isempty(A)
  error('Input image A is undefined or invalid.');
end
if ~isfloat(A) || ~sum([3] == size(A,3)) || ...
    min(A(:)) < 0 || max(A(:)) > 1
  error(['Input image A must be a double precision ',...
         'matrix of size NxMx3 on the closed ',...
         'interval [0,1].']);
end

% Verify the number of hierarchy levels
if ~exist('nLevels', 'var') || isempty(nLevels) || ...
    numel(nLevels) ~= 1 || nLevels < 1
  nLevels = 3;
end
nLevels = ceil(nLevels);

% Convert to Lab color space
A = rgb2lab(A);
L = A(:,:,1);

% Set parameters.
sigma_s = 2;
sigma_r = range(L(:))/10;

% Build the hierarchy.
B = zeros(size(A,1), size(A,2), nLevels);
B(:,:,1) = A(:,:,1);
for i = 2:nLevels
  A = bilateral(A, sigma_s, sigma_r);
  B(:,:,i) = A(:,:,1);
  sigma_s = sigma_s * 2;
  sigma_r = sigma_r / 2;
end

end