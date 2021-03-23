#' Determine if a set of columns constitute a candidate key for a dataset
#'
#' Description
#' Description
#' Description
#' Description
#'
#' @param dt data frame
#' @param cols character vector with column names
#' @details This function
#' @export
is_candidate_key <- function(dt, cols) {
  UseMethod("is_candidate_key")
}

is_candidate_key.data.table <- function(dt, cols) {
  anyDuplicated(dt[, ..cols]) == 0
}

is_candidate_key.default <- function(dt, cols) {
  anyDuplicated(dt[, match(cols, names(dt))]) == 0
}

#' Filter all rows that are not uniquely identified by a set of columns
#'
#' Description
#' Description
#' Description
#' Description
#'
#' @param dt data frame
#' @param cols character vector with column names
#' @details lorem ipsum
#' @export
filter_duplicated_rows <- function(dt, cols) {
  UseMethod("filter_duplicated_rows")
}


filter_duplicated_rows.data.table <- function(dt, cols) {
  index <- duplicated(dt[, ..cols], fromLast = TRUE) | duplicated(dt[, ..cols], fromLast = FALSE)
  dt[index, ]
}

filter_duplicated_rows.default <- function(dt, cols) {
  index <- duplicated(dt[, match(cols, names(dt))], fromLast = TRUE) | duplicated(dt[, match(cols, names(dt))], fromLast = FALSE)
  dt[index, ]
}
