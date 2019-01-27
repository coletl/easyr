#' Compare similarly named columns
#' @description Extract columns with similar names and output a list of separate data.frames for easy comparison.
#' The presence/absence of a prefix/suffix identifies similar colums,
#' and the default corresponds to default-data.table joining (e.g., \code{i.count}, \code{count}).
#' You can provide a list of column names for extraction instead.
#'
#' @param x a data.frame.
#' @param id_cols character vector of any ID columns to retain in each data.frame output.
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
#' x <- data.frame(id = paste0("row_", 1:10),
#'                 char = LETTERS[1:10],
#'                 i.char = sample(LETTERS[1:10]),
#'                 num = 1:10,
#'                 i.num = sample(1:10)
#'                 )
#'
#' comp_cols(x)
#' comp_cols(x, id_cols = "id", restrict = "mismatch")
#'
#' x[2, "char"] <- NA
#' x[2, "num"] <- NA
#' x[3, c("char", "i.char")] <- NA
#' x[5, c("num", "i.num")] <- NA
#'
#' comp_cols(x)
#' comp_cols(x, id_cols = "id", rm_na = TRUE)
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
      # Remove all-NA rows
      out <-
        Map(function(df, cols){
          all_na <- rowSums(is.na(df[ , cols])) == length(df[ , cols])
          df[ ! all_na, ]
        },
        df = out, cols = col_list)
    }


    switch(restrict,

           mismatch = {
             out <-
               Map(function(df, cols){
                 all_match <- sapply(df[ , cols],
                                     FUN = identical,
                                     df[ , cols[1]])
                 df[ ! all_match, ]
               },
               df = out, cols = col_list)
           },

           match = {
             out <-
               Map(function(df, cols){
                 all_match <- sapply(df[ , cols],
                                     FUN = identical,
                                     df[ , cols[1]])
                 df[all_match, ]
               },
               df = out, cols = col_list)
           }
    )

    return(out)
  }

