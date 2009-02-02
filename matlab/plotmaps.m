% setup the canonical domains in matlab

% add directories to the path
path(path,'sc')
path(path,'poly_stuff')

% create a grid
x_points=[-5:0.1:5]
y_points=[-5:0.1:5]
[X,Y]=meshgrid(x_points,y_points)

% find the internal points
internal_points=inpoly([X(:) Y(:)], poly)

% create a complex set of points and polygon
complex_points=complex(X(internal_points),Y(internal_points))
complex_poly=polygon(complex(poly(:,1),poly(:,2)))

% with rectmap, you need to specify the vertices but not the centre
f=rectmap(complex_poly, vertex_points)

% plot the map and the vertices
plot(evalinv(f,complex_points),'.')
hold on
plot(evalinv(f,complex_poly(vertex_points)),'s', 'MarkerFaceColor','g', 'MarkerEdgeColor','g')
hold off


