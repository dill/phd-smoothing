# using ARPACK via igraph

library(igraph)

f <- function(x, extra) as.vector(extra%*%t(t(x)))

A<-matrix(100*rnorm(1000000),1000,1000)

A<-A+t(A)


t1<-system.time(ap<-arpack(f,A, options=list(n=dim(A)[1], nev=2,which="LM",ncv=100), sym=TRUE))[3]

t2<-system.time(ei<-eigen(A))[3]

cat("ARPACK=",t1,"\n")
cat("eigen=",t2,"\n")

# check
#(A-ei$values[1]*diag(10))%*%ei$vector[,1]
#(A-ap$values[1]*diag(10))%*%ap$vector[,1]

