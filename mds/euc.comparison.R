# find the Euclidean distance between each pair of points, output to distance matrix


euc<-matrix(0, length(samp.mds$points[,1]),length(samp.mds$points[,1]))




# compute upper diagonal for the new point configuration
for(i in 1:length(samp.mds$points[,1])){
   for(j in i:length(samp.mds$points[,1])){
      euc[i,j]<-sqrt(
                  (samp.mds$points[i,1]-samp.mds$points[j,1])^2+
                  (samp.mds$points[i,2]-samp.mds$points[j,2])^2 )
   }
}

# find the lower diagonal
euc<-euc+t(euc)

difff<-D-euc

summary(as.vector(difff))

