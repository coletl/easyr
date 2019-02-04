#' Link data.tables by fuzzy string matching
#'
#' @description Finds the closest string match between two data.tables.
#' The default method computes Jaro-Winkler string distances using the \code{stringdist} package.
#' In cases with multiple closest matches, only the first match is reported.
#' @param a a source data.table
#' @param b a target data.table
#' @param acol column name in \code{a} to use for matching
#' @param bcol column name in \code{b} to use for matching
#' @param blocks an optional character vector of column names referring to any variables to be used for \emph{exact} blocking.
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
#' DTA <- data.table(block_a1 = sample(LETTERS[1:4], 20, TRUE),
#'                   block_a2 = sample(LETTERS[1:4], 20, TRUE),
#'                   fruit   = sample(stringr::fruit[1:12], 20, TRUE))
#'
#' DTB <- data.table(block_b1 = sample(LETTERS[1:4], 20, TRUE),
#'                   block_b2 = sample(LETTERS[1:4], 20, TRUE),
#'                   fruit   = sample(stringr::fruit[1:12], 20, TRUE))
#'
#'
#' a      <-  DTA
#' b      <-  DTB
#' acol   <-  "fruit"
#' bcol   <-  "fruit"
#' blocks <-  c(block_a1 = "block_b1", block_a2 = "block_b2")
#'
#' fuzzy_match(a, b, acol, bcol)
#' fuzzy_match(a, b, acol, bcol, blocks = blocks)


#' @export
fuzzy_match <-
  function(a, b, acol, bcol, blocks = NULL, method = "jw", ...) {

    if(length(blocks) > 0) {
      # Names of blocks argument are the a columns on which to block
      # The blocks object itself holds the names of the b columns on which to block

      # If no names on blocks object, assume the blocking-column names are the same
      # in both data sets
      if(is.null(names(blocks))) names(blocks) <- blocks

      b[ , names(blocks) := mget(blocks)]


      setkeyv(a, names(blocks))
      setkeyv(b, names(blocks))

      # Blocked distance computations
      sdm <-
        a[ ,
           .(sdmats =
               list(

                 stringdist::stringdistmatrix(get(acol),
                                              b[.BY, get(bcol)],
                                              useNames = TRUE, method = method...)
               )
           ),
           by = names(blocks)
           ]

      # Clean up: remove new columns from b
      b[ , names(blocks) := NULL]


      # Find the best matches in b for each block-acol combination
      sdm[ ,
           best_match :=
             list(
               lapply(sdmats,
                      function(sdmat) {
                        min_dist <- matrixStats::rowMins(sdmat)
                        best_match <- character()

                        for(ii in 1:length(min_dist)) best_match[ii] <- first(colnames(sdmat)[sdmat[ii, ] == min_dist[ii]])

                        setNames(
                          data.table(rownames(sdmat), best_match, min_dist),
                          c(acol, "best_match", "min_strdist")
                        )
                      }
               )
             )
           ]


      sdm <- tidyr::unnest(sdm, best_match)

    } else {
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

    }

    return(sdm)

  }
