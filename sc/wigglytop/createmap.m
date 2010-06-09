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

% original values
vertex_points=[1 2 3 18];
% trying something else
% vertex_points=[1 2 15 16]; % CRASH
%vertex_points=[1 2 14 17]; % CRASH

% do the mapping

% rectangle map
f=rectmap(polyv_p, vertex_points);

% crdt
%f=crrectmap(polyv_p);

% disk map
%f=crdiskmap(polyv_p);


%%%%%%%%%%%%%%%%%%%
% load the truth points
sample_points=csvread('wttruth.csv',1,0);

% copy to output
z_out_samp=sample_points(sample_points(:,4)==1,3);

% select the points we want 
sample_points=sample_points(sample_points(:,4)==1,1:2);

% make the data into a complex var
sample_points=complex(sample_points(:,1),sample_points(:,2));

% map those points
mapped_sample_points=evalinv(f,sample_points);

csvwrite('wttruemapped.csv',[real(mapped_sample_points),imag(mapped_sample_points),z_out_samp]);

