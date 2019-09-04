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

#' flighttime
#'
#' Function to simulate the response (flight time) from the paper helicopter experiment, using a semi-physical model based on dimensional analysis
#' @param w rotor width [0.03, 0.09]m
#' @param r rotor length [0.07, 0.12]m
#' @param t tail length [0.07, 0.12]m
#' @param d paper density [0.06, 0.12] kgm^-2
#' @param phi standard deviation of response distribution
#' @return Simulated helicopter flight time (seconds)
#' @details The flight time is simulated from a normal distribution with mean derived from a dimensional analysis regression model and standard deviation phi.
#'
#' Other parameters (theta0, theta1 and int) are regression parameters.
#' @examples
#' x <- c(0.03, 0.12, 0.12, 0.06)
#' flighttime(w = x[, 1], r = x[, 2], t = x[, 3], d = x[, 4])
#' @references
#' \url{https://www.paperhelicopterexperiment.com}
#'
#' Box, G. E. P. and P. Y. T. Liu (1999). Statistics as a catalyst to learning by scientific method part I - an example. Journal of Quality Technology 31, 1–15.
#'
#' Shen, W., T. Davis, D. K. J. Lin, and C. J. Nachtsheim (2014). Dimensional analysis and its applications in statistics. Journal of Quality Technology 46, 185–198.
#'
#' Woods, D. C., Overstall, A. M., Adamou, M. and Waite, T. W. (2017). Bayesian design of experiments for generalised linear models and dimensional analysis with industrial and scientific application (with discussion). Quality Engineering, 29, 91-118.
#' @export
flighttime <- function(w, r, t, d, theta0 = exp(0.102), theta1 = 1.9, phi = .1, int = 0) {
  m <- d * (2 * w * (r + 0.025) + t * 0.05)
  ET <- (theta0 * 5) / sqrt(9.8 * r) * ((1.2 * r ^ 3) / m) ^ theta1
  n <- length(w)
  stats::rnorm(n, mean = int + ET, sd = phi)
}

#' contr.twolevel
#'
#' Contrasts for two-level factors with levels -1 and +1 (recoding factor levels)
#'
#' @examples
#' x <- factor(letters[1:2])
#' contrasts(x) <- contr.twolevel()
#' @export
contr.twolevel <- function() {
  contr <- c(-1, 1)
  cbind(contr, deparse.level = 0)
}

#' Amatrix
#'
#' Calculates an alias matrix for a fractional factorial experiment
#' @param X1 model matrix for the assumed model
#' @param X2 model matrix for the additional terms in the true model
#' @return The alias matrix
#' @examples
#' library(FrF2)
#' design <- FrF2(8, 5, factor.names = paste0("x", 1:5))
#' X <- model.matrix(~(.) ^ 2, data = design)
#' X1 <- X[, 1:6]
#' X2 <- X[, 7:16]
#' round(Amatrix(X1, X2), 2)
#' @export
Amatrix<-function(X1, X2) {
  X1 <- as.matrix(X1)
  X2 <- as.matrix(X2)
  A <- solve(t(X1) %*% X1) %*% t(X1) %*% X2
  A
}
