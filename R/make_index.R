#' Cria vetor de datas
#'
#' Description
#' Description
#' Description
#' Description
#'
#' @param start data de inicio no formato YYYY-MM
#' @param h numero para determinar tamanho do vetor
#' @param x objeto para determinar tamanho do vetor
#' @param end data final no formato YYYY-MM
#' @param class classe de retorno do vetor
#' @details Somente h, x ou end devem ser especificados

#' @export
make_index <- function(start, h, x, end, class = c("yearmon", "Date")) {

  stopifnot(!missing(start), is.character(start), nchar(start) == 7, !missing(x) | !missing(end) | !missing(h))

  class <- match.arg(class)
  start_date <- as.Date(paste(start,"-01",sep=""))

  if(!missing(x)) {
    stopifnot(is.numeric(x), is.atomic(x))
    x_index <- seq.Date(start_date, along.with = x, by = "month")
  }

  if(!missing(h)) {
    stopifnot(is.numeric(h), is.atomic(h), length(h) == 1)
    if(h < 0) {
      h_index <- seq.Date(start_date, length.out = abs(h), by = "-1 month")
    } else {
      h_index <- seq.Date(start_date, length.out = h, by = "month")
    }

  }

  if(!missing(end)) {
    stopifnot(is.character(end), nchar(end) == 7)
    end_date <- as.Date(paste(end,"-01",sep=""))
    if(end_date < start_date) {
      end_index <- seq.Date(start_date, to = end_date, by = "-1 month")
    } else {
      end_index <- seq.Date(start_date, to = end_date, by = "month")
    }

  }

  if(!missing(x) & !missing(h)) {stopifnot(identical(x_index, h_index))}
  if(!missing(x) & !missing(end)) {stopifnot(identical(x_index, end_index))}
  if(!missing(h) & !missing(end)) {stopifnot(identical(h_index, end_index))}

  if(!missing(x)) {
    index <- x_index
  } else if(!missing(h)) {
    index <- h_index
  } else {
    index <- end_index
  }

  switch (class,
    yearmon = return(zoo::as.yearmon(substr(index, 1, 7))),
    Date = return(index)
  )

}

#' Cria datas (YYYY-MM-DD) a partir de anos e meses
#'
#' Description
#' Description
#' Description
#' Description
#'
#' @param x coluna ano no formato YYYY
#' @param y coluna mes no formato MM
#' @details O dia inserido será sempre 1

#' @export
make_date <- function(x, y) {
  paste(x, formatC(y, width = 2, flag = "0"), "01", sep = "-")
}
