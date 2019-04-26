#' @export
ah <- function(data) {
  suppressWarnings(data %>%
                     janitor::adorn_totals("row") %>%
                     janitor::adorn_percentages("col") %>%
                     janitor::adorn_pct_formatting() %>%
                     janitor::adorn_ns(position = "front"))
}

#' @export
av <- function(data) {
  suppressWarnings(data %>%
                     janitor::adorn_totals("row") %>%
                     adorn_pct_change())

}
