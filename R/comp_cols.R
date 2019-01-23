#' Compare similarly named columns
#' @description Extract columns with similar names and output a list of separate data.frames for easy comparison.
#' The presence/absence of a prefix/suffix identifies similar colums,
#' and the default corresponds to default-data.table joining (e.g., \code{i.count}, \code{count}).
#' You can provide a list of column names for extraction instead.
#'
#' @param x a data.frame.
#' @param id_cols any ID columns to retain in each data.frame output.
#' @param pattern a regular expression pattern to match the distinguishing prefix/suffix.
#' @param col_list a list of \code{character(2)} column-name pairs, Non-\code{NULL} values here take precedence over \code{pattern}.
#' @param restrict one of \code{"match"}, \code{"mismatch"}, or \code{FALSE}, specifying whether to restrict the output to matches only, mismatches only, or not at all (the default).
#' @param rm_allna logical indicating whether, in each set of comparison columns, all-\code{NA} rows should be removed.
#'
#' @return A list of data.frame objects.
#'
#' @examples
#' set.seed(575)
#'
#' x <- data.frame(char = LETTERS[1:10],
#'                 i.char = sample(LETTERS[1:10]),
#'                 num = 1:10,
#'                 i.num = sample(1:10)
#'                 )
#'
#' comp_cols(x)
#' comp_cols(x, restrict = "mismatch")
#'
#' x[2, 1] <- NA
#' x[2, 3] <- NA
#' x[3, 1:2] <- NA
#' x[5, 3:4] <- NA
#'
#' comp_cols(x)
#' comp_cols(x, rm_na = TRUE)
#'
#' @export

comp_cols <-
  function(x, id_cols = NULL, pattern = "^i\\.",
           col_list = NULL,
           restrict = FALSE,
           rm_na = FALSE) {

    if(is.null(col_list)) {
      icol <- grep(pattern, names(x), value = TRUE)
      xcol <- gsub(pattern, "", icol)

      col_list <- mapply(c, xcol, icol, SIMPLIFY = FALSE)
    }

    out <-
      lapply(col_list,
             function(cols) dplyr::select(x, c(id_cols, cols)))

    if(rm_na) {
      # Remove NA rows
      out <-
        Map(function(df, cols)
          df[
            is.na(df[[ cols[1] ]]) + is.na(df[[ cols[2] ]]) < ncol(df),
            ],
          df = out, cols = col_list)
    }


    switch(restrict,

           mismatch = {
             out <-
               Map(function(df, cols)
                 df[
                   df[[ cols[1] ]] != df[[ cols[2] ]],
                   ],
                 df = out, cols = col_list)
           },

           match = {
             out <-
               Map(function(df, cols)
                 df[
                   df[[ cols[1] ]] == df[[ cols[2] ]],
                   ],
                 df = out, cols = col_list)
           }
    )

    return(out)
  }

