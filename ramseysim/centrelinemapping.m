% file to map the centreline, for nullspace testing.

path(path,'sc')
path(path,'poly_stuff')
% really rough horsehoe shape from R
poly=[3.39967559926863 0.9; -0.9 0.9; -0.9 -0.9; 3.39967559926863 -0.9; 3.39967559926863 -0.1; -0.00825793454723323 -0.1; -0.00825793454723323 0.1; 3.39967559926863 0.1; ]

% make it into a complex polygon object suitable for sctoolbox
complex_poly=polygon(complex(poly(:,1),poly(:,2)))

% which vertices to map to the vertices of the rectangle
vertex_points=[1 4 5 8]

% create the map object
f=rectmap(complex_poly, vertex_points)

% load the data and format
thisdata=csvread('../ramseysim/centreline.csv',1,0)
thisdata_complex=complex(thisdata(:,1),thisdata(:,2))

% map them
remappeddata=evalinv(f,thisdata_complex)

% write out the file
csvwrite('centrelinemapped.csv',real(remappeddata),imag(remappeddata))
