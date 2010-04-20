# using ARPACK via igraph
# what kind of speed increase are we getting?


library(igraph)

f <- function(x, extra) as.vector(extra%*%t(t(x)))

t1<-0; t2<-0;

for(i in 1:100){
   A<-matrix(100*rnorm(40000),200,200)
   
   A<-A+t(A)
   
   
   t1<-t1+system.time(ap<-arpack(f,A, options=list(n=dim(A)[1], nev=2,which="LM",ncv=100), sym=TRUE))[3]
   
   t2<-t2+system.time(ei<-eigen(A))[3]
}

t1<-t1/100
t2<-t2/100

cat("ARPACK=",t1,"\n")
cat("eigen=",t2,"\n")


