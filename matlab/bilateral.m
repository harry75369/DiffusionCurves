function B = bilateral(A, s, r)
%BILATERAL Bilateral filter.
%   Input image A is a double precision matrix of size NxMx3 in CIE-Lab
%   colorspace. Input s is spatial width and r is range width.
%

% Verify that the input image exists and is valid.
if ~exist('A', 'var') || isempty(A)
  error('Input image A is undefined or invalid.');
end
if ~isfloat(A) || ~sum([3] == size(A,3))
  error('Input image A must be a double precision matrix of size NxMx3');
end

% Verify spatial and range width.
if ~exist('s', 'var') || isempty(s) || numel(s) ~= 1
  error('Input spatial width is undefined or invalid.');
end
if ~exist('r', 'var') || isempty(r) || numel(r) ~= 1
  error('Input range width is undefined or invalid.');
end

% Set parameters.
w = 3; % window size

% Pre-compute Gaussian spatial weights.
[X,Y] = meshgrid(-w:w,-w:w);
G = exp(-(X.^2+Y.^2)/(s^2));

% Create waitbar.
h = waitbar(0,'Applying bilateral filter...');
set(h,'Name','Bilateral Filter Progress');

dim = size(A);
B = zeros(dim);
for i = 1:dim(1)
  for j = 1:dim(2)
    % Extract local region.
    iMin = max(i-w,1);
    iMax = min(i+w,dim(1));
    jMin = max(j-w,1);
    jMax = min(j+w,dim(2));
    I = A(iMin:iMax,jMin:jMax,:);
    
    % Compute Gaussian intensity weights.
    dL = I(:,:,1)-A(i,j,1);
    da = I(:,:,2)-A(i,j,2);
    db = I(:,:,3)-A(i,j,3);
    H = exp(-(dL.^2+da.^2+db.^2)/(r^2));
    
    % Calculate bilateral filter response.
    F = H.*G((iMin:iMax)-i+w+1,(jMin:jMax)-j+w+1);
    norm_F = sum(F(:));
    B(i,j,1) = sum(sum(F.*I(:,:,1)))/norm_F;
    B(i,j,2) = sum(sum(F.*I(:,:,2)))/norm_F;
    B(i,j,3) = sum(sum(F.*I(:,:,3)))/norm_F;
  end
  waitbar(i/dim(1));
end

% Close waitbar.
close(h);

end