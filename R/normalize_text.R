#' @export
normalize_text <- function(x) {
  result <- x |>
    stringr::str_to_lower() |>
    stringi::stri_trans_general("latin-ascii") |> # acentos
    stringr::str_replace_all("[[:punct:]]", " ") |> # pontuacao
    stringr::str_squish() # espacos iniciais, finais e multiplos espacos
  result
}
