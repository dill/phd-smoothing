# thin plate spline functions

eta <- function(r) { 
   # thin plate spline basis functions
   ind <- r<=0 
   eta <- r 
   eta[!ind] <- r[!ind]^2*log(r[!ind])/(8*pi) 
   eta[ind] <- 0 
   eta
}

XSC <- function(x,xk=x,D.xxk=NULL,D.xkxk=NULL) { 
   # set up t.p.s., given covariates, x, and knots, xk
   n <- nrow(x);k <- nrow(xk) 
   X <- matrix(1,n,k+3) # tps model matrix 

   if(!is.null(D.xxk) & !is.null(D.xkxk)){
      for (j in 1:k) {
         r<-D.xxk[,j]
         X[,j] <- eta(r)
      }

   }else{
      for (j in 1:k) {
         r <- sqrt((x[,1]-xk[j,1])^2+(x[,2]-xk[j,2])^2) 
         X[,j] <- eta(r)
      }
   }

   X[,j+2] <- x[,1];X[,j+3] <- x[,2] 
   C <- matrix(0,3,k+3) # tps constraint matrix 
   S <- matrix(0,k+3,k+3)# tps penalty matrix 

   if(!is.null(D.xxk) & !is.null(D.xkxk)){
      for (i in 1:k) {
         C[1,i]<-1;C[2,i] <- xk[i,1];C[3,i] <- xk[i,2] 
         for (j in i:k){ 
            r<-D.xkxk[i,j]
            S[j,i]<-S[i,j]<-eta(r)
         }
      }
   }else{
      for (i in 1:k) {
         C[1,i]<-1;C[2,i] <- xk[i,1];C[3,i] <- xk[i,2] 
         for (j in i:k){ 
            S[j,i]<-S[i,j]<-eta(sqrt(sum((xk[i,]-xk[j,])^2))) 
         }
      }
   }
   list(X=X,S=S,C=C)
}

absorb.con <- function(X,S,C) { 
   # get constraint null space, Z...
   qrc <- qr(t(C)) # QR=C’, Q=[Y,Z] 
   m <- nrow(C);k <- ncol(X) 
   X <- t(qr.qty(qrc,t(X)))[,(m+1):k] # form XZ 
   # now form Z’SZ ... 
   S <- qr.qty(qrc,t(qr.qty(qrc,t(S))))[(m+1):k,(m+1):k]
   list(X=X,S=S,qrc=qrc)
}

fit.tps <- function(y,x,xk=x,lambda=0,D.xxk=NULL,D.xkxk=NULL) { 
   tp <- XSC(x,xk,D.xxk,D.xkxk)	# get tps matrices
   tp <- absorb.con(tp$X,tp$S,tp$C) # make unconstrained 
   ev <- eigen(tp$S,symmetric=TRUE) # get sqrt penalty, rS 
   rS <- ev$vectors%*%(ev$values^.5*t(ev$vectors)) 
   X <- rbind(tp$X,rS*sqrt(lambda)) # augmented model matrix 
   z <- c(y,rep(0,ncol(rS)))	# augmented data 
   beta <- coef(lm(z~X-1))	# fit model
   beta <- qr.qy(tp$qrc,c(0,0,0,beta)) # backtransform beta
}

eval.tps <- function(xp,beta,xk,D.xpxk=NULL) { 
   # evaluate tps at xp, given parameters, beta, and knots, xk.
   k <- nrow(xk);n <- nrow(xp) 
   f <- rep(beta[k+1],n)
   if(!is.null(D.xpxk)){
      for (i in 1:k) { 
         r<-D.xpxk[,i]
         f <- f + beta[i]*eta(r)
      } 
   }else{
      for (i in 1:k) { 
         r <- sqrt((xp[,1]-xk[i,1])^2+(xp[,2]-xk[i,2])^2) 
         f <- f + beta[i]*eta(r)
      } 
   }
   f <- f + beta[k+2]*xp[,1] + beta[k+3]*xp[,2]
}

