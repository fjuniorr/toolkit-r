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
tx_cresc <- function(x) {
  stopifnot(is.atomic(x), is.numeric(x))
  razao <- c(diff(x), NA) / x
  razao <- razao[-length(x)]
  razao <- c(NA, razao)
  razao
}

#' @export
tx_media_cresc <- function(x) {
  stopifnot(is.atomic(x), is.numeric(x))
  tx_media <- ((x[length(x)] / x[1]) ^ (1 / (length(x) - 1))) - 1
  tx_media
}
