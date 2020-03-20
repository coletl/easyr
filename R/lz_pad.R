#' @name lz_pad
#'
#' @title Pad with zeroes on the left
#'
#' @description This just calls \code{\link[stirngr]{str_pad}} to
#' pad each element of a character vector with zeros on the left side.
#' This is helpful when standardizing the length of numeric ID variables.
#'
#' @param string a vector.
#' @param width  minimum number of characters after padding.
#'
#' @return A character vector.
#' @export

lz_pad <-
  function(string, width){
    stringr::str_pad(string, width, side = "left", pad = "0")
  }
