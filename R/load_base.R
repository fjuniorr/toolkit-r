#' Carrega e mescla bases de dados da execução orçamentária
#'
#'
#' @param base identificador da base de dados (ie. exec_rec)
#' @param dir diretório que armazena as bases
#' @export
load_base <- function(base, dir = "~/../.execucao/") {
 
  file <- paste0(base, ".xlsx")
  path <- file.path(dir, file)
  
  metadados <- readxl::read_excel(path, sheet = "metadados")
  msg <- glue::glue_data(metadados,
                         "Base {base} atualizada em {substr(DATA_ATUALIZACAO, 1, 10)} com referência {ANO_REF}-{MES_REF}\n")

  message(msg)
  
  excel <- switch(base,
                  "exec_rec" = reest::ler_exec_rec(path),
                  reest::ler_exec_desp(path))
  
  ret <- rbind(get_base(base), excel)
  
  ret
  }



get_base <- function(base)
{
  e <- new.env()
  utils::data(list = base, package = "execucao", envir = e)
  e[[base]]
}
