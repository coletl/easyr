#' Check for coercion's NA introduction
#' @description Detect where coercing a vector introduces \code{NA}s.
#' @param x a vector.
#' @param FUN a function for type coercion.
#'
#' @return A logical vector indicating where \code{NA}s were introduced.

#' @examples
#' x <-  c("5", "12", "zero", NA, "0")
#' check <- detect_na_intro(x, as.numeric)
#' data.frame(x, check)
#' @export

detect_na_intro <-
  function(x, FUN) {
    pre_na <- is.na(x)
    all_na <-
      withCallingHandlers(is.na(FUN(x)),
                          warning = function(w) {
                            if (conditionMessage(w) == "NAs introduced by coercion")
                              invokeRestart("muffleWarning")
                          }
      )

    new_na <- all_na
    new_na[pre_na] <- FALSE

    return(new_na)
  }

