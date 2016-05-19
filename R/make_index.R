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
