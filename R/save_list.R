#' Write list elements to disk as separate objects
#' @name save_list
#'
#' @param x a list.
#' @param dir a directory in which to save the list elements.
#' @param ext a file extension.
#' @param names a vector of file names.
#' @param cores number of cores to use in \code{\link{mcparallel}}. The default (1) runs in series.
#' @param ... further arguments to pass to \code{\link{saveRDS}} or \code{\link[data.table]{fwrite}}.
#' @seealso \code{\link{saveRDS}} and \code{\link[data.table]{fwrite}}.
#' @examples
#'
#' tmp_dir <- tempdir()
#' df <- data.frame(char = LETTERS[1:10], num = 1:10)
#' ldf <- replicate(3, df, simplify = FALSE)
#'
#' save_list(ldf, dir = tmp_dir, ext = ".csv")
#'
#' list.files(tmp_dir)
#' unlink(tmp_dir, recursive = TRUE)


save_list <-
  function(x, dir, ext = c(".rds", ".csv"), names = NULL,
           cores = 1, ...){
    if(!file.exists(dir)) dir.create(dir)

    if(is.null(names)) names <- if(is.null(names(x))) 1:length(x) else names(x)

    ext <- match.arg(ext)

    invisible(

      switch(ext,
             .rds = parallel::mclapply(names,
                                       function(nm) saveRDS(x[[nm]],
                                                            file.path(dir, paste0(nm, ext)),
                                                            ...),
                                       mc.cores = cores
             ),
             .csv = parallel::mclapply(names,
                                       function(nm) data.table::fwrite(x[[nm]],
                                                                       file = file.path(dir, paste0(nm, ext)),
                                                                       ...),
                                       mc.cores = cores
             )
      )

    )
  }
