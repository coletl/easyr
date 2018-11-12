#' @name set_cols
#'
#' @title Internally looped \code{set()}
#'
#' @description A convenience function implementing \code{\link[data.table]{set}}
#' looped over columns of a \code{data.frame}.
#' Modification is fast (by reference) but cannot include grouping operations.
#'
#' @param x a \code{data.frame}.
#' @param FUN a function to be applied to all \code{x[i, j]}.
#' @param i integer indices indicating rows to update.
#' @param j column names or integer indices indicating columns to update.
#' The default updates all columns.
#'
#' @seealso \code{\link[data.table]{:=}}.
#' @export


set_cols <-
  function(x, FUN, i = NULL, j = NULL){
    if(is.null(j)) j <- seq.int(1, length(x))

    for(col in j) data.table::set(x, i, j = col,
                                  value = FUN(x[[col]]))
  }

df <- data.frame(a = letters, b = LETTERS, c = LETTERS,
                 stringsAsFactors = FALSE)
set_cols(df, FUN = tolower)
