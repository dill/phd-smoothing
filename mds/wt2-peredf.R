source("wt2-intexp.R")

X11()
par(mfrow=c(1,2))

###
# pull design matrix, do the QR
mod<-b.tp
Xd<-model.matrix(mod)
Xd.qr<-qr(Xd)
R<-qr.R(Xd.qr)

# pull the smoother matrix
S<-mod$smooth[[1]]$S[[1]]
S<-cbind(S,rep(0,139))
S<-rbind(S,rep(0,140))

# 
ei<-eigen(t(solve(R))%*%S%*%solve(R))
s<-1+mod$sp*ei$values
ss<-1/s
plot(rev(ss),ylab="EDF per parameter",xlab="parameter index")
abline(v=20,col="red")
abline(v=40,col="red")

mod<-b.mdstp
Xd<-model.matrix(mod)
Xd.qr<-qr(Xd)
R<-qr.R(Xd.qr)

# pull the smoother matrix
S<-mod$smooth[[1]]$S[[1]]
S<-cbind(S,rep(0,139))
S<-rbind(S,rep(0,140))

# 
ei<-eigen(t(solve(R))%*%S%*%solve(R))
s<-1+mod$sp*ei$values
ss<-1/s
plot(rev(ss),ylab="EDF per parameter",xlab="parameter index")
abline(v=20,col="red")
abline(v=40,col="red")

