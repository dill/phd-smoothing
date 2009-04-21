% can be run from this directory

path(path,'../matlab/sc')
path(path,'../matlab/poly_stuff')

% read in the vertices
polyv=csvread('figverts.csv',0,0);

% create the polygon
polyv_c=complex(polyv(:,1),polyv(:,2));
polyv_p=polygon(complex(polyv(:,1),polyv(:,2)));

% which vertices on the polygon are vertices on the rectangle
% this is a bit arbitrary
vertex_points=[1 2 21 12];

% do the mapping

% rectangle map
%f=rectmap(polyv_p, vertex_points);

% crdt rectangle
f=crrectmap(polyv_p);


%%%%%%%%%%%%%%%%%%%
% load the truth points
sample_points=csvread('wt2truth.csv',1,0);

% copy to output
z_out_samp=sample_points(sample_points(:,4)==1,3);

% select the points we want 
sample_points=sample_points(sample_points(:,4)==1,1:2);

% make the data into a complex var
sample_points=complex(sample_points(:,1),sample_points(:,2));

% map those points
mapped_sample_points=evalinv(f,sample_points);

csvwrite('wt2truemapped.csv',[real(mapped_sample_points),imag(mapped_sample_points),z_out_samp]);

