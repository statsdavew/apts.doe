#' is.minusplus
#'
#' Simple utlity function to check if a number is in the set \{-1, +1\}
#' or if every element of a vector is in this set
#' @param x a number or vector
#' @export
is.minusplus <- function(x) {
  if(identical(length(x), as.integer(1))) return(x %in% c(-1, 1))
  test <- NULL
  for(i in 1:length(x)) test[i] <- ifelse(x[i] %in% c(-1, 1), T, F)
  return(as.logical(prod(test)))
}

#' materials.response
#'
#' Function that simulates the response from a materials wear testing experiment
#' @param x a vector of length six giving the values in \{-1,+1\} for each of the factors
#' @examples
#' x <- c(-1, +1, -1, +1, +1, -1)
#' materials.response(x)
#' @export
materials.response <- function(x) {
  if(!is.minusplus(x) || !identical(length(x), as.integer(6)))
    stop("x must contain six values, each taking value either -1, +1")
  names(x) <- NULL
  x <- as.numeric(x)
  d <- data.frame(t(x))
  xd <- model.matrix(~(.) ^ 2, d)
  betav <- c(53.53833122, -7.68959634, -1.87550556, 6.81307057, -2.08587135, -0.06510761,
             0.94808958, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1.34943664, -0.64495778, 0,
             0.60303180, 0, -5.19617780)
  rnorm(1, xd %*% betav, 3)
}

#' borehole
#'
#' Function that simulates the response from mathematical model of water flow rate
#' through a borehole
#' @param x a vector of length eight giving the values for each of the inputs.
#' See the reference url for the ranges of each input
#' @examples
#' x <- c(0.12650025, 897.8873, 97266.21, 1041.3891, 76.17152, 745.6965, 1608.609, 11853.055)
#' borehole(x)
#' @references
#' \url{http://www.sfu.ca/~ssurjano/}
#' @export
borehole <- function(xx)
{
  ##########################################################################
  #
  # BOREHOLE FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it
  # and/or modify it under the terms of the GNU General Public License as
  # published by the Free Software Foundation; version 2.0 of the License.
  # Accordingly, this program is distributed in the hope that it will be
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # OUTPUT AND INPUT:
  #
  # y  = water flow rate
  # xx = c(rw, r, Tu, Hu, Tl, Hl, L, Kw)
  #
  ##########################################################################
  if(!is.vector(xx) || is.list(xx) || !identical(length(xx), as.integer(8))) stop("Input should be a vector of length 8")

  rw <- xx[1]
  r  <- xx[2]
  Tu <- xx[3]
  Hu <- xx[4]
  Tl <- xx[5]
  Hl <- xx[6]
  L  <- xx[7]
  Kw <- xx[8]

  frac1 <- 2 * pi * Tu * (Hu-Hl)

  frac2a <- 2*L*Tu / (log(r/rw)*rw^2*Kw)
  frac2b <- Tu / Tl
  frac2 <- log(r/rw) * (1+frac2a+frac2b)

  y <- frac1 / frac2
  return(y)
}
