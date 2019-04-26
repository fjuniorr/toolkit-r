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
  x <- as.numeric(x)
  stopifnot(is.atomic(x), is.numeric(x))
  razao <- c(diff(x), NA) / x
  razao <- razao[-length(x)]
  razao <- c(NA, razao)
  razao
}

#' @export
tx_media_cresc <- function(x) {
  x <- as.numeric(x)
  stopifnot(is.atomic(x), is.numeric(x))
  tx_media <- ((x[length(x)] / x[1]) ^ (1 / (length(x) - 1))) - 1
  tx_media
}

#' @export
adorn_pct_change <- function(dat) {
  # from janitor::adorn_percentages
  # This function excludes the first column of the input data.frame, 
  # assuming that it contains a descriptive variable.
  # SECOND OPTION from xltabr
  # all columns before the last character/factor columns are considered labels 
  # and therefore not included in the calculations and analyses
  label_var <- names(dat[, 1])
  
  # all numeric columns
  stopifnot(all(unlist(lapply(dat[, -1], is.numeric))))
  
  long <- melt(dat, id.vars = label_var, variable.name = "variable", value.name = "value")
  
  
  # alternatives
  # https://stackoverflow.com/questions/19824601/how-calculate-growth-rate-in-long-format-data-frame
  # https://github.com/krlmlr/kimisc
  long[, pct_change := f(tx_cresc(value), type = "%") , by = label_var]
  
  long <- long[, c(label_var, "variable", "pct_change"), with = FALSE]
  
  dcast(long, ... ~ variable, value.var = "pct_change")
}