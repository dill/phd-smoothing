% can be run from this directory

path(path,'../matlab/sc')
path(path,'../matlab/poly_stuff')

% read in the vertices
polyv=csvread('figverts.csv',0,0)

%polyv =
%
%    8.5921    0.3638
%    8.8262    6.1035
%    3.4040    6.2340
%    2.8969    8.7995
%   -3.6176    7.7559
%   -2.8374    2.7554
%   -9.3518    0.9291
%   -6.2701   -6.2021
%    5.9005   -7.6370
%    8.5921    0.3638


% create the polygon
polyv_c=complex(polyv(:,1),polyv(:,2))
polyv_p=polygon(complex(polyv(:,1),polyv(:,2)))

% which vertices on the polygon are vertices on the rectangle
% this is a bit arbitrary
vertex_points=[1 6 8 9]

% do the mapping
f=rectmap(polyv_p, vertex_points)



%%%%%%%%%%%%%%%%%%%
% load the truth points
sample_points=csvread('fig9truth.csv',1,0)

% make the data into a complex var
sample_points=complex(sample_points(:,1),sample_points(:,2))

% map those points
mapped_sample_points=evalinv(f,sample_points)

csvwrite('fig9truemapped.csv',[real(mapped_sample_points),imag(mapped_sample_points)])


%%%%%%%%%%%%%%%%%%%%%%
% load the points where the random sample was taken from R
sample_points=csvread('fig9out.csv',1,0)

% make the data into a complex var
sample_points=complex(sample_points(:,1),sample_points(:,2))

% map those points
mapped_sample_points=evalinv(f,sample_points)

csvwrite('fig9mapped.csv',[real(mapped_sample_points),imag(mapped_sample_points)])



