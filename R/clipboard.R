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
copia <- function(x) {
  write.table(x,"clipboard",sep="\t", row.names = FALSE, dec = ",")
}

#' @export
cola <- function() {
  read.table("clipboard", sep="\t", header = TRUE, dec = ",")
}

#' @export
w <- function(x) {
  obj <- deparse(substitute(x))
  file <- paste0(obj, ".csv")
  path <- file.path(getwd(), file)
  write.csv2(x, path, row.names = FALSE)
}
