## apts.doe package
## documentation - dcw 15-0-18

#' @title cirfab
#' @description \eqn{2^4} full factorial design and response from an experiment fabricating integrated circuit boards.
#' Each factor has coded levels \code{-1,+1}
#' @format A data frame with 16 rows and 5 variables:
#' \describe{
#'   \item{\code{x1}}{rotation method}
#'   \item{\code{x2}}{nozzle position}
#'   \item{\code{x3}}{deposition temperature}
#'   \item{\code{x4}}{deposition time}
#'   \item{\code{ybar}}{thickness of the epitaxial layer (micrometres) - average from six wafers}
#'}
#' @source Wu, C.F.J. and Hamada, M.S. (2009). Experiments: Planning, Analysis, and Optimization. 2nd ed. Wiley, New York. Page 155
"cirfab"


#' @title ssd
#' @description A Bayesian D-optimal supersaturated design with 16 factors and 10 runs
#' Each factor has coded levels \code{-1,+1}
#' @format A data frame with 10 rows and 16 variables:
"ssd"

#' @title ebm
#' @description A maximin Latin hypercube design with two variables and one response
#' @format A data frame with 20 rows and 3 variables:
#' \describe{
#'   \item{\code{y}}{mean temperature}
#'   \item{\code{x1}}{solar constant}
#'   \item{\code{x2}}{non-ice albedo}
#'}
#' @source The response variable was obtained by running the Energy Balance Model available from \url{https://wiki.aston.ac.uk/foswiki/bin/view/MUCM/SurfebmModel}
"ebm"
