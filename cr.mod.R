# This is a modification of the routines from mgcv


smooth.construct.cr.smooth.spec<-function(object,data,knots)
# this routine is the constructor for cubic regression spline basis objects
# It takes a cubic regression spline specification object and returns the 
# corresponding basis object.
{ shrink <- attr(object,"shrink")
  x <- data[[object$term]]
  nx<-length(x)
  if (is.null(knots)) ok <- FALSE
  else
  { k <- knots[[object$term]]
    if (is.null(k)) ok <- FALSE
    else ok<-TRUE
  }

  if (object$bs.dim < 0) object$bs.dim <- 10 ## default

  if (object$bs.dim <3) { object$bs.dim <- 3
    warning("basis dimension, k, increased to minimum possible\n")
  }

  nk <- object$bs.dim
  if (!ok) { k <- rep(0,nk);k[2]<- -1}
  if (length(k)!=nk) stop("number of supplied knots != k for a cr smooth")

  X <- rep(0,nx*nk);S<-rep(0,nk*nk);C<-rep(0,nk);control<-0

  if (length(unique(x))<nk)
  { msg <- paste(object$term," has insufficient unique values to support ",
                 nk," knots: reduce k.",sep="")
    stop(msg)
  }


  #################################################
  # Things start to get weird here
  # map x onto the unit disk
  map.return<-sc.map.backwards(x,nvertices,betam,nptsq,qwork,accuracy,prevertices,polyvertices,complex.scale.factor,wc)
  x<-map.return$

  #################################################

  oo <- .C(C_construct_cr,as.double(x),as.integer(nx),as.double(k),
           as.integer(nk),as.double(X),as.double(S),
           as.double(C),as.integer(control))

  object$X <- matrix(oo[[5]],nx,nk)

  object$S<-list()     # only return penalty if term not fixed
  if (!object$fixed)
  { object$S[[1]] <- matrix(oo[[6]],nk,nk)
    object$S[[1]]<-(object$S[[1]]+t(object$S[[1]]))/2 # ensure exact symmetry
    if (!is.null(shrink)) # then add shrinkage term to penalty 
    { norm <- mean(object$S[[1]]^2)^0.5
      object$S[[1]] <- object$S[[1]] + diag(nk)*norm*abs(shrink)
    }
  }
  if (is.null(shrink)) {
  object$rank<-nk-2
  } else object$rank <- nk   # penalty rank

  object$df<-object$bs.dim # degrees of freedom,  unconstrained and unpenalized
  object$null.space.dim <- 2
  object$xp <- oo[[3]]  # knot positions 
  class(object) <- "cr.smooth"
  object
}

Predict.matrix.cr.smooth<-function(object,data)
# this is the prediction method for a cubic regression spline
{ ##x <- get.var(object$term,data)
  x <- data[[object$term]]
  if (length(x)<1) stop("no data to predict at")
  nx<-length(x)
  nk<-object$bs.dim
  X <- rep(0,nx*nk);S<-rep(0,nk*nk);C<-rep(0,nk);control<-0

  oo <- .C(C_construct_cr,as.double(x),as.integer(nx),as.double(object$xp),
            as.integer(object$bs.dim),as.double(X),as.double(S),
                   as.double(C),as.integer(control))
  X<-matrix(oo[[5]],nx,nk) # the prediction matrix

  X
}

