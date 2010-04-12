# using ARPACK via igraph

library(igraph)


f <- function(x, extra) as.vector(extra%*%t(t(x)))

arpack(f,diag(3), options=list(n=3, nev=2,which="LM"), sym=TRUE)$vectors

