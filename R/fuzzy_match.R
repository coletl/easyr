#' Link data.tables by fuzzy string matching
#'
#' @description Finds the closest string match between two data.tables.
#' The default method computes Jaro-Winkler string distances using the \code{stringdist} package.
#' In cases with multiple closest matches, only the first match is reported.
#' @param a a source data.table
#' @param b a target data.table
#' @param acol column name in \code{a} to use for matching
#' @param bcol column name in \code{b} to use for matching
#' @param method method for \code{\link[stringdist]{stringdistmatrix}}
#'
#' @return a data.table containing any blocking columns,
#' the source column,
#' the closest match in the target column,
#' and the string distance for that match.
#'
#' @seealso
#' \link[stringdist]{stringdistmatrix}
#'
#' @examples
#' library(data.table)
#'
#' set.seed(575)
#' DTA <- data.table(block1 = sample(LETTERS[1:4], 20, TRUE),
#'                   block2 = sample(LETTERS[1:4], 20, TRUE),
#'                   fruit   = sample(stringr::fruit[1:12], 20, TRUE))
#'
#' DTB <- data.table(block1 = sample(LETTERS[1:4], 20, TRUE),
#'                   block2 = sample(LETTERS[1:4], 20, TRUE),
#'                   fruit   = sample(stringr::fruit[1:12], 20, TRUE))
#'
#' fuzzy_match(DTA, DTB, "fruit", "fruit")
#'
#' setkey(DTA, block1, block2)
#' setkey(DTB, block1, block2)
#'
#' DTA[ , fuzzy_match(.SD, b = DTB[.BY], "fruit", "fruit"),
#'        by = .(block1, block2)]
#'
#' @export

fuzzy_match <-
  function(a, b, acol, bcol, method = "jw", ...) {
    # Unblocked distance computations
    sdmat <- stringdist::stringdistmatrix(a[[acol]], b[[bcol]], useNames = TRUE,
                                          method = method, ...)
    min_dist <- matrixStats::rowMins(sdmat)

    best_match <- character()
    for(ii in 1:length(min_dist)) best_match[ii] <- first(colnames(sdmat)[sdmat[ii, ] == min_dist[ii]])

    sdm <-
      setNames(
        data.table(rownames(sdmat), best_match, min_dist),
        c(acol, "best_match", "min_strdist")
      )
    return(sdm)
  }
