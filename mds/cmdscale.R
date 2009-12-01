#  File src/library/stats/R/cmdscale.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

cmdscale <- function (d, k = 2){
	 x <- as.matrix(d^2)
    storage.mode(x) <- "double"
    ## doubly center x in-place
    .C(R_dblcen, x, as.integer(n), DUP = FALSE)

    e <- eigen(-x/2, symmetric = TRUE)
    ev <- e$values[1L:k]

    points <- e$vectors[, 1L:k, drop = FALSE] %*% diag(sqrt(ev), k)

    list(points=points, eig=ev, x=x)
}
