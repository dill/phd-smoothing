# figures for the cexample figures
# run from the mds directory

par(mfrow=c(2,2))

# original coordinates for the points
X.orig<-matrix(c(1.5,1+sqrt(3)/2,1,1,2,1),2,3)
X.orig<-t(X.orig)

plot(X.orig,xlab="",ylab="",pch=19)


## Do the MDS
# distance matrix
D3<-matrix(c(0,1,1,1,0,1,1,1,0),3,3)

# centring matrix, H
ones<-t(t(c(1,1,1)))
H<-diag(1,3,3)-1/3*(ones%*%t(ones))

S<- -1/2*H%*%D3%*%H

# eigen decompose
U<-eigen(S)$vectors
lambda<-diag(eigen(S)$values)
lambda.inv<-diag(1/eigen(S)$values)

X<-U%*%chol(lambda)

plot(X,xlab="",ylab="",pch=19)


## insert new point
d2<-t(t(c(1,0,1)))
d2<-d2-mean(d2)

xnew<-1/2*lambda.inv%*%t(X)%*%d2


plot(X,xlab="",ylab="",pch=19)
points(x=xnew[1:2,][1],y=xnew[1:2,][2])


## insert everything
X.new<-1/2*lambda.inv%*%t(X)%*%S

plot(t(X.new[1:2,]),xlab="",ylab="",pch=19)
