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

