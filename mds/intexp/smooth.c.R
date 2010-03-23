# thin plate spline with squash
# taken from smooth.r in mgcv

smooth.construct.tp.smooth.spec<-function(object,data,knots)
## The constructor for a t.p.r.s. basis object.
{ shrink <- attr(object,"shrink")
  ## deal with possible extra arguments of "tp" type smooth
  xtra <- list()

  if (is.null(object$xt$max.knots)) xtra$max.knots <- 3000
  else xtra$max.knots <- object$xt$max.knots
  if (is.null(object$xt$seed)) xtra$seed <- 1
  else xtra$seed <- object$xt$seed
  ## now collect predictors
  x<-array(0,0)
  shift<-array(0,object$dim)
  for (i in 1:object$dim)
  { ## xx <- get.var(object$term[[i]],data)
    xx <- data[[object$term[i]]]
    shift[i]<-mean(xx)  # centre covariates
    xx <- xx - shift[i]
    if (i==1) n <- length(xx) else
    if (n!=length(xx)) stop("arguments of smooth not same dimension")
    x<-c(x,xx)
  }
  if (is.null(knots)) {knt<-0;nk<-0}
  else
  { knt<-array(0,0)
    for (i in 1:object$dim)
    { dum <- knots[[object$term[i]]]-shift[i]
      if (is.null(dum)) {knt<-0;nk<-0;break} # no valid knots for this term
      knt <- c(knt,dum)
      nk0 <- length(dum)
      if (i > 1 && nk != nk0)
      stop("components of knots relating to a single smooth must be of same length")
      nk <- nk0
    }
  }
  if (nk>n) { nk <- 0
  warning("more knots than data in a tp term: knots ignored.")}
  ## deal with possibility of large data set
  if (nk==0 && n>xtra$max.knots) { ## then there *may* be too many data  
    xu <- uniquecombs(matrix(x,n,object$dim)) ## find the unique `locations'
    nu <- nrow(xu)  ## number of unique locations
    if (nu>xtra$max.knots) { ## then there is really a problem 
      seed <- get(".Random.seed",envir=.GlobalEnv) ## store RNG seed
      kind <- RNGkind(NULL)
      RNGkind("default","default")
      set.seed(xtra$seed) ## ensure repeatability
      nk <- xtra$max.knots ## going to create nk knots
      ind <- sample(1:nu,nk,replace=FALSE)  ## by sampling these rows from xu
      knt <- as.numeric(xu[ind,])  ## ... like this
      RNGkind(kind[1],kind[2])
      assign(".Random.seed",seed,envir=.GlobalEnv) ## RNG behaves as if it had not been used
    }
  } ## end of large data set handling
  if (object$bs.dim[1]<0) object$bs.dim <- 10*3^(object$dim-1) # auto-initialize basis dimension
  object$p.order[is.na(object$p.order)] <- 0 ## auto-initialize
  k<-object$bs.dim
  M<-null.space.dimension(object$dim,object$p.order)
  if (k<M+1) # essential or construct_tprs will segfault, as tprs_setup does this
  { k<-M+1
    object$bs.dim<-k
    warning("basis dimension, k, increased to minimum possible\n")
  }

  X<-array(0,n*k)
  S<-array(0,k*k)

  UZ<-array(0,(n+M)*k)
  Xu<-x
  C<-array(0,k)
  nXu<-0
  oo<-.C(C_construct_tprs,as.double(x),as.integer(object$dim),as.integer(n),as.double(knt),as.integer(nk),
               as.integer(object$p.order[1]),as.integer(object$bs.dim),X=as.double(X),S=as.double(S),
               UZ=as.double(UZ),Xu=as.double(Xu),n.Xu=as.integer(nXu),C=as.double(C))
  object$X<-matrix(oo$X,n,k)                   # model matrix

  object$S<-list()
  if (!object$fixed)
  { object$S[[1]]<-matrix(oo$S,k,k)         # penalty matrix
    object$S[[1]]<-(object$S[[1]]+t(object$S[[1]]))/2 # ensure exact symmetry
    if (!is.null(shrink)) # then add shrinkage term to penalty 
    { ## pre- 1.5 code the identity term could dominate the small eigenvales
      ## and really mess up the penalty...
      ## norm <- mean(object$S[[1]]^2)^0.5
      ## object$S[[1]] <- object$S[[1]] + diag(k)*norm*abs(shrink)

      ## Modify the penalty by increasing the penalty on the 
      ## unpenalized space from zero... 
      es <- eigen(object$S[[1]],symmetric=TRUE)
      ## now add a penalty on the penalty null space
      es$values[(k-M+1):k] <- es$values[k-M]*shrink
      ## ... so penalty on null space is still less than that on range space.
      object$S[[1]] <- es$vectors%*%(as.numeric(es$values)*t(es$vectors))
    }
  }
  UZ.len <- (oo$n.Xu+M)*k
  object$UZ<-matrix(oo$UZ[1:UZ.len],oo$n.Xu+M,k)         # truncated basis matrix
  Xu.len <- oo$n.Xu*object$dim
  object$Xu<-matrix(oo$Xu[1:Xu.len],oo$n.Xu,object$dim)  # unique covariate combinations

  object$df<-object$bs.dim                   # DoF unconstrained and unpenalized
  object$shift<-shift                          # covariate shifts
  if (is.null(shrink)) {
    object$rank <- k-M
  } else object$rank <- k                             # penalty rank
  object$null.space.dim<-M

  class(object)<-"tprs.smooth"

  # recreate the S object

  # use finite difference to find the second derivatives

  # set some limits
  lims<-seq(a,b,length.out=100)

  # make the midpoints here

  midp<-data.frame()
  midp2<-data.frame() # as above plus epsilon

  d1<-Predict.matrix(object,midp)
  d2<-Predict.matrix(object,midp2)

  fd<-(d1-d2)/eps

  # then sum fd




  object
}

