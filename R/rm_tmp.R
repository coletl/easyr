#' @name rm_tmp
#' @title Remove temporary files
#'
#' @description Remove files in the \R session's temporary directory.
#' This function is useful in conjunction with packages like \code{raster} and \code{aws.s3}, which frequently write files to \code{tempdir()}.

#' @examples
#' tmp_csv <- tempfile(fileext = rep(c(".csv", ""), 10))
#' lapply(tmp_csv, function(path) write.csv("", path))
#'
#' list.files(tempdir())
#'
#' rm_tmp(pattern = "\\.csv")
#' list.files(tempdir())
#'
#' rm_tmp()
#' list.files(tempdir())
#'
#' @export

rm_tmp <- function(pattern = NULL) {
  tmpd <- tempdir()

  tmp_all <- list.files(tmpd, full = TRUE, recursive = TRUE)
  tmp_match <- list.files(tmpd, full = TRUE, recursive = TRUE,
                          pattern = pattern)
  rm_count <- sum(file.remove(tmp_match))

  sprintf("%d of %d temporary files removed.", rm_count, length(tmp_all))
}
