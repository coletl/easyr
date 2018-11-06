#' Detect and remove outliers
#' @name outliers
#' @aliases detect_outliers
#' @aliases rm_outliers
#'
#' @description Detect and remove outliers, as defined by quantile probabilities.
#'
#' @param x a numeric vector.
#' @param df a \code{data.frame}.
#' @param probs for \code{detect_outliers()}, a length-2 numeric vector of probabilities. For \code{rm_outliers()}, the same or a list of such vectors. See \code{\link[base]{quantile}}.
#' @param na.rm logical, whether to remove \code{NA} values before computing quantiles.
#' @param incbounds logical, whether boundary values should be interpreted as inclusive. See \code{\link[data.table]{between}}.
#' @param ... quoted column names in which to search for outliers, or a vector of column names.
#'
#' @examples
#' df <- data.frame(num1 = 1:1e3, num2 = sample(1:1e3))
#'
#' df$num1[detect_outliers(df$num1, probs = c(0.025, 0.975))]
#'
#' identical(rm_outliers(df, c("num1", "num2")),
#'           rm_outliers(df, "num1", "num2")
#'           )
#' @seealso \code{\link[base]{quantile}} and \code{\link[data.table]{between}}.
#' @export

#' @rdname outliers
#' @export
detect_outliers <- function(x, probs, na.rm = FALSE, incbounds = TRUE) {
  if(length(probs) > 2)
    stop("probs must be a numeric vector of length two (or a list of such vectors if called from rm_outliers())")

  qntl <- quantile(x, probs, na.rm)

  ! data.table::inrange(x, min(qntl), max(qntl), incbounds)
}

#' @rdname outliers
#' @export
rm_outliers <-
  function(df, ..., probs = c(0.025, 0.975),
           na.rm = FALSE, incbounds = TRUE) {
    dots <- list(...)

    # If all ... arguments are character, interpret them as a column names
    if(all(vapply(dots, is.character, logical(1))))
      dots <- lapply(unlist(dots), function(name) df[[name]])

    # Unless probs is a list, replicate for as many columns as
    if(!is.list(probs)) probs <- rep_len(list(probs), length(dots))

    outliers_list <-
      Map(f = detect_outliers,
          x = dots, probs = probs,
          na.rm = na.rm, incbounds = incbounds)

    # Find where any element of any vector is an outlier
    outliers_any <- do.call(pmax.int, outliers_list)

    df[!as.logical(outliers_any), ]
  }
