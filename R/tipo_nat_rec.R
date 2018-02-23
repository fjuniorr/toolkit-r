tipo_nat_rec <- function(x) {
  
  if(!all(nchar(x) == 13)) {
    stop("Classificação da Receita não possui 13 dígitos")
  }
  
  x <- format(x, scientific = FALSE, trim = TRUE)
  
  # remove "." das naturezas para permitir consultas nat(RECEITA_COD, 1.1)
  x <- gsub("\\.", "", x)
  tipo_rec <- substr(x, 8, 8)
  
  unlist(lapply(tipo_rec, lookup_tipo_nat_rec))
}


lookup_tipo_nat_rec <- function(x) {
  switch (x,
          "0" = "0.agregadora",
          "1" = "1.principal",
          "2" = "2.mjm",
          "3" = "3.divida_ativa",
          "4" = "4.mjm_divida_ativa",
          NA_character_
  )
}


