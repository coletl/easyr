#' @name %notin%
#' @aliases notin
#' @title Binary operator for non-inclusion
#'
#' @description This is just a convenience function for \code{! x %in% y}.
#'
#' @details See the note on matching \code{NA} in \code{\link[base]{match}}.
#'
#' @examples
#' x <- c(LETTERS[1:5], NA)
#' y <- c(LETTERS[4:8], NA)
#'
#' data.frame(x, y,
#'            not_in = x %notin% y)
#' @export

`%notin%` <- function(x, y) ! x %in% y
