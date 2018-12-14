#' Order substrings of a character vector
#' @name order_substr
#' @description Split a character vector, reorder the substrings, and paste back together.
#' @param x a character vector.
#' @param split a regular expressin by which to split. See \link{Details}.
#' @param perl logical, whether to interpret \code{split} as a Perl-style regular expression.
#' @param collapse a character string with which to collapse the re-ordered sub-strings.
#' @param reverse logical, whether to reverse the ordering.
#' @return A character vector with substrings ordered alphabetically and re-collapsed.
#' @details By default, \code{order_substr()} splits each string of a character vector at any commas,
#'          forward slashes, hyphens, and whitespace, regardless of whether these characters
#'          are surrounded by whitespace.
#'
#'          Alternative regular expressions for splitting can be used via \code{split}.
#'          Other strings may be used to separate the alphabetized output via \code{collapse}.
#'
#' @note    Cleaning with \code{order_substr()} works poorly when substrings are inconsistently separated
#'          by the split pattern. For example, the vector of tuna species below hyphenates \code{yellow-fin}
#'          but leaves \code{bluefin} as a single word. Therefore, with the default split patterns,
#'          \code{order_substr()} splits \code{yellow-fin} prior to alphabetization.
#'
#' @examples
#' tuna <- c("tuna,skipjack", "tuna  , bluefin", "yellow-fin - tuna", "tuna,   albacore")
#' order_substr(tuna)
#'
#' colors <- c("green/red", "yellow / blue", "orange purple")
#' order_substr(colors, collapse = "-", reverse = TRUE)
#' @export

order_substr <-
  function(x, split = "\\s*,\\s*|\\s*/\\s*|\\s*-\\s*|\\s+",
           perl = TRUE, collapse = " ", reverse = FALSE) {
    # Locate NA values to add back in later
    na_ind <- which(is.na(x))

    # Split each element into its own character vector at user-defined split patterns
    tmp <- strsplit(x, split = split, perl = perl)
    # Sort the substrings alphabetically and collapse.
    tmp2 <- vapply(tmp,
                   function(x) paste(sort(x, decreasing = reverse),
                                     collapse = collapse
                   ),
                   FUN.VALUE = character(1)
    )
    # Trim leading and trailing whitespace
    tmp3 <- gsub(tmp2, patt = "^\\s|\\s$", rep = "")

    # Add NA values back in
    tmp3[na_ind] <- NA_character_

    return(tmp3)
  }
