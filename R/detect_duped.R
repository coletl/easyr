#' @name detect_duped
#' @title Test for duplicates
#'
#' @description Test for duplicates in one or a combination of vectors.
#'
#' @details
#' This function SHOULD NOT be used in places of \code{duplicated()} and DOES NOT RETURN DUPLICATES.
#' Instead, \code{detect_duped()} executes the common pattern of \code{vector %in% vector[duplicated(vector)]}.
#'
#' Providing multiple vector arguments to \code{...} concatenates the vectors element-wise
#' before testing for duplicates in the new, collapsed vector.
#'
#' @section Profiling:
#' I think the bottleneck here is the call to \code{interaction()}.
#' It might be worth benchmarking this against \code{paste()}.
#' It's possible that R does some internal operations when creating factors to optimize memory usage and speed up things like duplicated().
#' In that case, it's probably best to profile the entire function when testing,
#' rather than just compare the runtimes for different functions.
#'
#' @param ... vectors to concatenate.
#' @param sep a string used to when concatenating the vectors. See \code{\link[base]{interaction}}.
#'
#' @return A logical vector corresponding to values that are duplicates \emph{or are duplicated}.
#'
#' @seealso \code{\link[base]{duplicated}} and \code{\link[base]{interaction}}.
#'
#' @examples
#' state <- c("CA", "IL", "FL", "CA")
#' cd    <- c(22, 11, 22, 22)
#'
#' data.frame(state, cd,
#'            dup = detect_duped(state, cd))
#'
#' @export


detect_duped <-
  function(..., sep = "-^-") {

    if(length(list(...)) > 1) combs <- interaction(..., drop = TRUE, sep = sep)
    else combs <- as.vector(...)

    dups <- unique(combs[duplicated(combs)])

    out <- combs %in% dups

    message(
      sprintf("%d instances of %d duplicated elements",
              sum(out), length(dups)
      )
    )

    return(out)
  }
