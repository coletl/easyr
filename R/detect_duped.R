#' @name detect_duped
#' @title Test for duplicates
#'
#' @description Test for duplicates in one or a combination of vectors.
#'
#' @details
#' This function \strong{\emph{should not}} be used in places of \code{duplicated()} and \strong{\emph{does not return duplicates}}.
#' Instead, \code{detect_duped()} executes the common pattern
#'
#' \code{vector \%in\% vector[duplicated(vector)]}.
#'
#' Providing multiple vector arguments to \code{...} concatenates the vectors element-wise
#' before testing for duplicates in the new vector.
#'
#' @param ... vectors to concatenate.
#' @param sep a string used to when concatenating the vectors. See \code{\link[base]{interaction}}.
#' @param incomparables FALSE or a vector of incomparable---i.e., never-duplicate---values. See \code{\link[base]{duplicated}}.
#'
#' @return A logical vector corresponding to values that are duplicates \emph{or are duplicated}.
#'
#' @seealso \code{\link[base]{duplicated}} and \code{\link[base]{interaction}}.
#'
#' @examples
#' state <- c("CA", "CA", "FL", "CA", "FL", "FL")
#' cd    <- c(22, 11, 22, 22, NA, NA)
#'
#' data.frame(state, cd,
#'            dup = detect_duped(state, cd),
#'            dup2 = detect_duped(state, cd, incomparables = NA))
#'
#' @export


detect_duped <-
  function(..., sep = "-^-", incomparables = FALSE) {
    if(length(list(...)) > 1) combs <- paste(..., sep = sep)
    else combs <- as.vector(...)

    dups <- unique(combs[duplicated(combs)])

    out <- combs %in% dups

    message(
      sprintf("%d instances of %d duplicated elements",
              sum(out), length(dups)
      )
    )

    if(!na_dupe) {
      na_ind <- which(is.na(combs))
      out[na_ind] <- NA
    }

    return(out)
  }
