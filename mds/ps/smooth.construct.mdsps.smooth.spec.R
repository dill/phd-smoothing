"smooth.construct.mdsps.smooth.spec"<-function (object, data, knots) 
{

    require(splines)
    if (length(object$p.order) == 1) 
        m <- rep(object$p.order, 2)
    else m <- object$p.order
    m[is.na(m)] <- 2
    object$p.order <- m
    if (object$bs.dim < 0) 
        object$bs.dim <- max(10, m[1] + 1)
    nk <- object$bs.dim - m[1]
    if (nk <= 0) 
        stop("basis dimension too small for b-spline order")
    x <- data[[object$term]]
    k <- knots[[object$term]]
    if (is.null(k)) {
        xl <- min(x)
        xu <- max(x)
    }
    else if (length(k) == 2) {
        xl <- min(k)
        xu <- max(k)
        if (xl > min(x) || xu < max(x)) 
            stop("knot range does not include data")
    }
    if (is.null(k) || length(k) == 2) {
        xr <- xu - xl
        xl <- xl - xr * 0.001
        xu <- xu + xr * 0.001
        dx <- (xu - xl)/(nk - 1)
        k <- seq(xl - dx * (m[1] + 1), xu + dx * (m[1] + 1), 
            length = nk + 2 * m[1] + 2)
    }
    if (object$bs.dim < 0) 
        object$bs.dim <- max(10, m[1] + 1)
    nk <- object$bs.dim - m[1]
    if (nk <= 0) 
        stop("basis dimension too small for b-spline order")
    x <- data[[object$term]]
    k <- knots[[object$term]]
    if (is.null(k)) {
        xl <- min(x)
        xu <- max(x)
    }
    else if (length(k) == 2) {
        xl <- min(k)
        xu <- max(k)
        if (xl > min(x) || xu < max(x)) 
            stop("knot range does not include data")
    }
    if (is.null(k) || length(k) == 2) {
        xr <- xu - xl
        xl <- xl - xr * 0.001
        xu <- xu + xr * 0.001
        dx <- (xu - xl)/(nk - 1)
        k <- seq(xl - dx * (m[1] + 1), xu + dx * (m[1] + 1), 
            length = nk + 2 * m[1] + 2)
    }
    else {
        if (length(k) != nk + 2 * m[1] + 2) 
            stop(paste("there should be ", nk + 2 * m[1] + 2, 
                " supplied knots"))
    }
    object$X <- spline.des(k, x, m[1] + 2, x * 0)$design
    if (!is.null(k)) {
        if (sum(colSums(object$X) == 0) > 0) 
            warning("knot range is so wide that there is *no* information about some basis coefficients")
    }
    S <- diag(object$bs.dim)
    if (m[2]) 
        for (i in 1:m[2]) S <- diff(S)
    object$S <- list(t(S) %*% S)
    object$S[[1]] <- (object$S[[1]] + t(object$S[[1]]))/2
    object$rank <- object$bs.dim - m[2]
    object$null.space.dim <- m[2]
    object$knots <- k
    object$m <- m
    class(object) <- "pspline.smooth"
    object
}

