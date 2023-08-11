#' Title
#'
#' Description
#' Description
#' Description
#' Description
#'
#' @param param1 description
#' @param param1 description
#' @details Detais
#' Detais
#' Detais
#' @export
f <- function(x, type = c("ORIGINAL", "%", "k", "M", "B")) {
  type <- match.arg(type)

  value <- switch(type,
                  "ORIGINAL" = formattable::accounting(x, big.mark = ".", decimal.mark = ","),
                  "%" = formattable::percent(x, digits = 1, big.mark = ".", decimal.mark = ","),
                  "k" = formattable::accounting(x/1000, digits = 1 , big.mark = ".", decimal.mark = ","),
                  "M" = formattable::accounting(x/1000000, digits = 1 , big.mark = ".", decimal.mark = ","),
                  "B" = formattable::accounting(x/1000000000, digits = 1 , big.mark = ".", decimal.mark = ",")
  )
  value
}

#' @export
pp <- function(dt) {
  result <- data.table::copy(dt)
  cols_to_format <- names(result)[sapply(result, is.numeric) & sapply(result, function(x) any(x >= 100000, na.rm = TRUE))]

  # format those columns
  result[, (cols_to_format) := lapply(.SD, function(x) formattable::accounting(x, big.mark = ".", decimal.mark = ",")), .SDcols = cols_to_format]
  result[]
}
