# using ARPACK via igraph

library(igraph)


f <- function(x, extra) extra%*%t(t(x))

arpack(f,diag(3), options=list(n=2, nev=12,which="LM"), sym=TRUE)$values

