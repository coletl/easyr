#' Compare similarly named columns
#' @description Extract columns with similar names and output a list of separate data.frames for easy comparison.
#' The presence/absence of a prefix/suffix identifies similar colums,
#' and the default corresponds to default-data.table joining (e.g., \code{i.count}, \code{count}).
#' You can provide a list of column names for extraction instead.
#'
#' @param x a data.frame.
#' @param id_cols any ID columns to retain in each data.frame output.
#' @param pattern a regular expression pattern to match the distinguishing prefix/suffix
#' @param col_list a list of grouped column names, which takes precedence over \code{pattern}.
#'
#' @return A list of data.frame objects.

#' @examples
#' x <-  c("5", "12", "zero", NA, "0")
#' check <- detect_na_intro(x, as.numeric)
#' data.frame(x, check)
#' @export

comp_cols <-
  function(x, id_cols = NULL, pattern = "^i\\.", col_list = NULL) {

    if(is.null(col_list)) {
      icol <- grep(pattern, names(x), value = TRUE)
      xcol <- gsub(pattern, "", icol)

      col_list <- mapply(c, xcol, icol, SIMPLIFY = FALSE)
    }

    lapply(col_list, function(cols) dplyr::select(x, c(id_cols, cols)))
  }
