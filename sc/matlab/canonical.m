% setup the canonical domains in matlab

plotcount=1
pcount=1

%%% square
poly=[-3 -3; 3 -3; 3 3;-3 3]
vertex_points=[1 2 3 4]
plotmaps

%%% triangle
%-3 -3; 3 -3; 0 3;
% Can't do this with rectmap! Not enough vertices!

%%% Ramsey's horse shoe
poly= [-2.5 -0.5; 2.5 -0.5; 3.5 0.5; 3.5 2; 2.5 3; -2.5 3; -2.5 1.5; 2 1.5; 2 1; -2.5 1]
vertex_points=[1 6 7 10]
plotmaps


%%% wiggly top 
%poly = [-3 -2; 3 -2; 3 2; 2.5 0.5; 2 1; 1.5 0.5; 1.5 1.5; 1 0.5; 0.5 1.5; 0 0.5; 0 2; -0.5 0.5; -1 2; -1 0.5; -1.5 1; -2 0.5; -2 1; -2.5 0.5; -2.5 2; -3 0.5;]
%vertex_points=[1 2 3 19]
%plotmaps

%%% spiked wiggly top 
poly = [-3 -2; 3 -2; 3 0.5; 2.5 1; 2.5 0.5; 2 1; 1.5 0.5; 1 1; 0.5 0.5; 0 1; -0.5 0.5; -0.5 1; -1 0.5; -2 1.5; -2.5 3; -3 3.5; -2.5 1.5; -3 0.5;]
vertex_points=[1 2 3 18]
plotmaps


%%% kite 
poly= [0 -2; 0.5 1; 1.5 2; 0 3.5; -1.5 2; -0.5 1;]
vertex_points=[1 3 4 5]
plotmaps


%%% beak 
poly = [-0.5 -3; 1.5 -3; 1.5 3; -0.5 3; -0.5 1; -2.5 2; -1 0; -2.5 -2.5; -0.5 -1; ]
vertex_points=[1 2 3 4]
plotmaps


%%% kink 
poly = [-3.5 0.5; -1 0.5; 0 -0.5; 1 0.5; 3.5 0.5; 3.5 1.5; 1 1.5; 0 0.5; -1 1.5; -3.5 1.5;]
vertex_points=[1 5 6 10]
plotmaps

%%% lobe
%poly = [-3 -1.5; -2 -1; -2 0; -1.5 1.5; -1 2.5; -0.5 3.5; 1.5 3; 2.5 1.5; 2.5 0; 3.5 -0.5; 3 -2.5; 1 -3; -1 -2; 0 0.5; 0 2; -1 1; -1.5 -1; -2 -2.5;]
%vertex_points=[1 8 9 18]
%plotmaps

