#' @name prop_na
#'
#' @title Calculate the proportion of missing values
#'
#' @description
#'
#' @param x a vector.
#'
#' @return A scalar. For \code{prop_na} the proportion of missing  values. For \code{count_na} a count of missing values
#'
#' @export
prop_na <- function(x) mean(is.na(x))

#' @rdname prop_na
#' @export
count_na <- function(x) sum(is.na(x))
