# routine to generate mv-normal data over a polygon
generate.poly.data<-function(polyvertices){

   # load MASS
   require(MASS)
   


   # create the data
   bivn <- mvrnorm(2500, mu = c(0, 0), Sigma = matrix(c(1, 0, 0, 1), 2))
   bivn.kde <- kde2d(bivn[,1], bivn[,2], n =50)



}
