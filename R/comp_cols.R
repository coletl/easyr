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
#' @param mismatch_only return only rows with different values in column pairs.
#' @param match_only return only rows with the same values in column pairs.
#' @param rm_na remove rows with \code{NA} in each set of comparison columns.
#'
#' @return A list of data.frame objects.
#'
#' @examples
#' x <- data.frame(char = LETTERS[1:10],
#'                 i.char = sample(LETTERS[1:10]),
#'                 num = 1:10,
#'                 i.num = sample(1:10)
#'                 )
#' comp_cols(x)
#'
#' @export

comp_cols <-
  function(x, id_cols = NULL, pattern = "^i\\.",
           col_list = NULL,
           mismatch_only = FALSE, match_only = FALSE,
           rm_na = FALSE) {

    stopifnot(mismatch_only + match_only < 2)

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
            is.na(df[[ cols[1] ]]) + is.na(df[[ cols[2] ]]) < 2,
            ],
          df = out, cols = col_list)
    }

    if(mismatch_only) {
      # Remove matches
      out <-
        Map(function(df, cols)
          df[
            df[[ cols[1] ]] != df[[ cols[2] ]],
            ],
          df = out, cols = col_list)
    }

    if(match_only) {
      # Remove matches
      out <-
        Map(function(df, cols)
          df[
            df[[ cols[1] ]] == df[[ cols[2] ]],
            ],
          df = out, cols = col_list)
    }

    return(out)
  }
