#' Inverse hyperbolic sine
#' @description Compute the inverse hyperbolic sine.
#'
#' @param x a numeric or complex vector.
#'
#' @return A numeric vector of transformed values.
#' @examples
#' x <- 1:1e4
#'
#' plot(ihs(x), pch = 20, cex = 0.2, col = "red")
#' points(log(x), pch = 20, cex = 0.1, col = "black")
#'
#' x <- rnorm(1e5, mean = 0, sd = 100) ^ 2
#' plot(density(x))
#'
#' plot(density(ihs(x)))
#' lines(density(log1p(x)), col = "blue")
#' lines(density(log(x)), col = "red")

ihs <- function(x) {
  y <- log(x + sqrt(x^2 + 1))
  return(y)
}
