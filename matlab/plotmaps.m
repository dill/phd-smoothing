% setup the canonical domains in matlab

% add directories to the path
path(path,'sc')
path(path,'poly_stuff')

% reset plotcount
plotcount=1

% create a grid
x_points=[-5:0.25:5]
y_points=[-5:0.25:5]
[X,Y]=meshgrid(x_points,y_points)

% find the internal points
internal_points=inpoly([X(:) Y(:)], poly)

% create a complex set of points and polygon
complex_points=complex(X(internal_points),Y(internal_points))
complex_poly=polygon(complex(poly(:,1),poly(:,2)))

% with rectmap, you need to specify the vertices but not the centre
f=rectmap(complex_poly, vertex_points)

% make corrections if needed
[w,a]=scfix(f,vertex(complex_poly),angle(complex_poly),vertex_points)
complex_poly=polygon(w,a)

% plot the original domain
subplot(1,2,plotcount)
plot(complex_points,'.', 'MarkerFaceColor','k', 'MarkerEdgeColor','k')
hold on
plot(complex_poly(vertex_points),'s', 'MarkerFaceColor','g', 'MarkerEdgeColor','g')
plot(complex_poly,[ ])
hold off

% increment plotcount
plotcount=plotcount+1

% plot the map and the vertices
subplot(1,2,plotcount)
plot(evalinv(f,complex_points),'.', 'MarkerFaceColor','k', 'MarkerEdgeColor','k')
hold on
plot(evalinv(f,complex_poly(vertex_points)),'s', 'MarkerFaceColor','g', 'MarkerEdgeColor','g')
plot(evalinv(f,complex_poly),[ ])
hold off

% print out to a file
print('-dpsc2', strcat('matlab-test-',char(48+pcount),'.ps'))

% increment
pcount=pcount+1

