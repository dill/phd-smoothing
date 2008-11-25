# Find the centre of a polygon
poly.centre<-function(polygon){
   # Input should be the vertices of the polygon as a complex vector.

   re.centre<-1/length(Re(polygon)) * sum(Re(polygon))
   im.centre<-1/length(Im(polygon)) * sum(Im(polygon))

   return(complex(length(re.centre),re.centre,im.centre))

}
