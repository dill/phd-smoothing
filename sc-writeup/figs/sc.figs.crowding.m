% crowding fixed in matlab

% add directories to the path
path(path,'sc');
path(path,'poly_stuff');

% create the polygon
pol_r=[7.696700 6.897026 4.732053 4.322464 5.102634 5.258668 5.960821 5.804787 5.161147 2.430550 2.000000 3.074191 7.053060 7.540666 7.696700];
pol_i=[8.877934 9.486692 9.725847 8.182211 8.443108 7.747385 8.030022 7.421264 6.442904 7.464747 4.290511 2.000000 2.000000 4.116580 8.877934];
poly=polygon(complex(pol_r,pol_i));



% create a grid
x_points=[-2:0.25:8];
y_points=[-2:0.25:10];
[X,Y]=meshgrid(x_points,y_points);

% find the internal points
internal_points=inpoly([X(:) Y(:)], [pol_r; pol_i]');

% create a complex set of points and polygon
complex_points=complex(X(internal_points),Y(internal_points));

complex_poly=poly

f=crdiskmap(complex_poly)

% plot the map and the vertices
plot(evalinv(f,complex_points),'.', 'MarkerFaceColor','k', 'MarkerEdgeColor','k')
hold on
plot(evalinv(f,complex(pol_r,pol_i)),'s', 'MarkerFaceColor','g', 'MarkerEdgeColor','g')
plot(evalinv(f,complex_poly),[ ])
hold off


