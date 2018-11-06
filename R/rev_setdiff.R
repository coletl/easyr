#' Reverse set difference
#' @description Same as \code{\link[base]{setdiff}} with argument order reversed.
#' @param x,y  vectors.
#'
#' @details This is a convenience function useful when \code{x} or \code{y} is long/cumbersome to type.
#' @return The output of \code{setdiff(y, x)}.
#' @seealso \code{\link[base]{setdiff}}.
#' @examples
#' x <- LETTERS[1:10]
#' y <- LETTERS[5:14]
#'
#' setdiff(x, y)
#' rev_setdiff(y, x)
#' rev_setdiff(x, y)
#'
#' @export

rev_setdiff <- function(x, y) setdiff(y, x)

