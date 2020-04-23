#' Switch out missing values
#' @name switch_na
#' @aliases zero_na
#' @aliases na_zero
#' @aliases null_na
#' @param x a vector.
#' @param unlist logical, whether to unlist the result.
#' @param ... further arguments to pass to \code{unlist()}.
#'
#' @examples
#' x <- sample(c(0:5, NA), 10, replace = TRUE)
#' rbind(x,
#'       zero_na = zero_na(x),
#'       na_zero = na_zero(x)
#'       )
#'
#' y <- list(1, 0, NULL, 1, 0)
#' null_na(y)
#' @export

#' @rdname na_switch
#' @return For \code{zero_na}, \code{0} values replaced with \code{NA}.
#' @export
zero_na <- function(x) {
  x[x == 0] <- NA
  return(x)
  }

#' @rdname na_switch
#' @return For \code{na_zero}, \code{NA} values replaced with \code{0}.
#' @export
na_zero <- function(x) {
  x[is.na(x)] <- 0
  return(x)
  }

#' @rdname na_switch
#' @return For \code{null_na}, \code{NULL} values replaced with \code{NA}.
#' @export
null_na <- function(x, unlist = FALSE, ...) {
  x[vapply(x, is.null, FUN.VALUE = logical(1))] <- NA
  if(unlist) x <- unlist(x, ...)

  return(x)
  }

#' @rdname na_switch
#' @return For \code{empty_na}, empty strings (\code{""}) replaced with \code{NA}.
#' @export
empty_na <- function(x, unlist = FALSE, ...) {
  x[x == ""] <- NA

  return(x)
}

#' @rdname na_switch
#' @return For \code{to_na}, arbitrary values replaced with \code{NA}.
#' @export
to_na <- function(x, values) {
  x[x %in% values] <- NA

  return(x)
  }

