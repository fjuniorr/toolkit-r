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
prop_table <- function(data, formula) {
  rhs <- as.character(f_rhs(formula))
  col_labels <- rhs[!grepl("+", rhs, fixed = TRUE)]
  if(col_labels == ".") {col_labels = NULL}

  lhs <- as.character(f_lhs(formula))
  row_labels <- lhs[!grepl("+", lhs, fixed = TRUE)]

  dta <- data[, c(row_labels, col_labels), drop = F]



  #
  # tbl <- dcast(data,
  #              formula,
  #              fun.aggregate = length,
  #              value.var = row_labels[1])
  #
  # DF <- dcast(data,
  #             formula,
  #             fun.aggregate = length,
  #             value.var = row_labels[1])
  #
  # rownames(DF) <- DF[[row_labels]]
  # TOTAL <- sum
  # tbl <- addmargins(as.table(as.matrix(DF[-1])), 1, FUN = TOTAL)
  #
  # prop.table(as.matrix(tbl))

}


x <- data.frame(Language=c("C++", "Java", "Python"),
                Files=c(4009, 210, 35),
                LOC=c(15328,876, 200),
                stringsAsFactors=FALSE)

rownames(x) <- x$Language
Total <- sum
xa <- addmargins(as.table(as.matrix(x[-1])), 1, FUN = Total)
