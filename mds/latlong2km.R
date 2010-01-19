# convert latitude longitude to kilometers from a base point

latlong2km<-function(lats,longs,base){

   # define the average radius of the Earth
   R<-6371

   # find distance from the lats and longs from the base
   dlats<-lats-base$lat
   dlongs<-longs-base$long

   # do the calculation
   a<-sin(dlats/2)^2 + cos(lats)*cos(base$lat)*sin(dlongs/2)^2
   c<-2*atan2(sqrt(a),sqrt(1-a))
   
   R*c

}



