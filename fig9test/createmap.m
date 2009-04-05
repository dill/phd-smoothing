path(path,'sc')
path(path,'poly_stuff')
polyv=csvread('../fig9sim/figverts.csv',0,0)

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

polyv_c=complex(polyv(:,1),polyv(:,2))

polyv_p=polygon(complex(polyv(:,1),polyv(:,2)))




