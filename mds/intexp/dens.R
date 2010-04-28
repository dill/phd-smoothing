# calculate the density estimate for the integration
# adjustment.
dens<-function(old.grid,old.points, new.grid, new.points){
   # old.grid - the grid we are integrating over in the old domain
   # old.points - points in the old domain that tell us the density
   # new.grid - the grid we are integrating over in the new domain
   # new.points - points in the new domain that tell us the density

   # we assume that the grid are evenly spaced!

   # first calculate the old grid density
   # old grid delta
   gdel<-diff(old.grid)[1]
   # old grid offset
   goff<-old.grid[1]
   A0<-floor((old.points-goff)/gdel)

   # then the new grid density
   # new grid delta
   gdel<-diff(new.grid)[1]
   # new grid offset
   goff<-new.grid[1]

   AM<-floor((new.points-goff)/gdel)

   return(A0/AM)
}
