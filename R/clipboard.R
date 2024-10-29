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
w <- function(x, name = NULL) {
  if(is.null(name)) {
    obj <- deparse(substitute(x))
  } else {
    obj <- name
  }
  file <- paste0(obj, ".xlsx")
  path <- file.path(getwd(), file)
  writexl::write_xlsx(x, path)
}

#' @export
cp <- function(dt) {
  knitr::kable(dt, format = "markdown") |> clipr::write_clip()  
}