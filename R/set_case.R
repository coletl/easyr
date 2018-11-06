#' Set all character columns of a data.table to a type case
#' @name set_case
#' @aliases set_upper
#' @aliases set_lower
#'
#' @param DT a \code{data.table}, or something coercible to one. See \code{\link[data.table]{setDT}}.
#' @examples
#'
#' df <- data.frame(a = letters, b = LETTERS,
#'                  stringsAsFactors = FALSE)
#'
#' set_lower(df)
#' set_upper(df)
#'
#' @seealso \code{\link[base]{tolower}} and \code{\link[base]{toupper}}.
#' @export

#' @rdname set_case
#' @export
set_upper <-
  function(DT){
    require(data.table)

    is_char <- vapply(DT, is.character, logical(1))
    char_cols <- names(DT)[which(is_char)]

    for(col in char_cols) set(DT, i = NULL, j = col, value = toupper(DT[[col]]))
  }

#' @rdname set_case
#' @export
set_lower <-
  function(DT){
    require(data.table)

    is_char <- vapply(DT, is.character, logical(1))
    char_cols <- names(DT)[which(is_char)]

    for(col in char_cols) set(DT, i = NULL, j = col, value = tolower(DT[[col]]))
  }
