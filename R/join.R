#' Mescla de múltiplas bases
#'
#' Função para realizar mescla de bases que sigam as convenções de nome das
#' colunas da AID
#'
#' @param ... bases separadas por virgulas que deverão ser mescladas
#' @param by vetor com as colunas presentes em todas as bases que serão utilizadas
#'  para mescla. Se \code{NULL} serão utilizadas as colunas comuns que não forem
#'  utilizadas como \code{value.var}
#' @param value.var variáveis de valor que serão agregadas pela função \code{sum}
#' @param regex \code{value.var} deve ser interpretado como uma expressão regular?
#' @param bind argumento que indica se as bases deverão ser empilhadas (rows) ou não (columns)
#' @param idcol argumento repassado a função \code{\link{rbindlist}}
#' @details A coluna .id somente pode ser adicionada a base de dados final
#'  caso as bases sejam empilhadas (bind = "rows").
#'  Além disso, caso as bases sejam informadas com nome (ie. join(BASE = dados)),
#'  essa informação será utilizada para identificação das bases na coluna .id.
#' @examples
#' receita <- data.frame(ano = c(2014, 2015), vlr_rec = c(100, 250))
#' despesa <- data.frame(ano = c(2014, 2015), vlr_desp = c(50, 25))
#'
#' join(receita, despesa)
#' ##     ano vlr_rec vlr_desp
#' ## 1: 2014    100       0
#' ## 2: 2015    250       0
#' ## 3: 2014      0      50
#' ## 4: 2015      0      25
#'
#' join(receita, despesa, idcol = TRUE)
#' ##    .id  ano vlr_rec vlr_desp
#' ## 1:   1 2014    100       0
#' ## 2:   1 2015    250       0
#' ## 3:   2 2014      0      50
#' ## 4:   2 2015      0      25
#'
#' join(REC = receita, DESP = despesa, idcol = TRUE)
#' ##     .id  ano vlr_rec vlr_desp
#' ## 1:  REC 2014    100       0
#' ## 2:  REC 2015    250       0
#' ## 3: DESP 2014      0      50
#' ## 4: DESP 2015      0      25
#'
#' join(REC = receita, DESP = despesa, idcol = "base")
#' ##    base  ano vlr_rec vlr_desp
#' ## 1:  REC 2014    100       0
#' ## 2:  REC 2015    250       0
#' ## 3: DESP 2014      0      50
#' ## 4: DESP 2015      0      25
#'
#' #--------------------------------------------------------------------
#' exec <- data.frame(ano = c(2014, 2015), vlr_liq = c(100, 250))
#' rp <- data.frame(ano = c(2014, 2015), vlr_liq_rp = c(50, 25))
#'
#' join(exec, rp, value.var = "vlr_liq")
#' ##     ano vlr_liq vlr_liq_rp
#' ## 1: 2014    100         0
#' ## 2: 2015    250         0
#' ## 3: 2014      0        50
#' ## 4: 2015      0        25
#'
#' join(exec, rp, value.var = "vlr_liq", regex = FALSE)
#' ##     ANO VL_LIQ
#' ## 1: 2014    100
#' ## 2: 2015    250
#' ## 3: 2014      0
#' ## 4: 2015      0

#' @export
join <- function(..., by = NULL, value.var = "^vlr_", regex = TRUE, bind = c("rows", "columns"), idcol = NULL) {
  #--------------------------------------------------------------------
  # validacao inputs
  x <- list(...)
  x <- lapply(x, to_data_table)
  bind <- match.arg(bind)

  if(bind == "columns" && idcol == TRUE) {
    stop("A coluna .id somente pode ser inserida quando bind == rows")
  }

  if(is.null(by)) {
    by <- Reduce(intersect1, lapply(x, names)) # colunas comuns
  }

  if(!all(unlist(lapply(x, function(x) {all(by %in% names(x))})))) {
    msg <- paste("A(s) coluna(s)", paste0(by, collapse = ", "), "nao estao presentes em todas as bases.")
    stop(msg)
  }

  if(regex == TRUE) {
    columns <- unique(unlist(lapply(x, names)))
    value.var <- grep(value.var, columns, value = TRUE)
  }
  #--------------------------------------------------------------------
  # as colunas valores nao devem ser utilizadas na chave
  by <- setdiff(by, value.var)
  #--------------------------------------------------------------------
  # agregacao para garantir as bases nao possuem linhas duplicadas
  l <- lapply(x, aggregate1, by = by, value.var = value.var)
  #--------------------------------------------------------------------
  # merge
  base <- switch(bind,
                 "rows" = rbindlist(l, fill = TRUE, idcol = idcol),
                 "columns" = Reduce(function(x, y) merge(x, y, by = by, all = TRUE), l)
  )

  #--------------------------------------------------------------------
  # NA nas colunas de valor devem ser tratados como zero
  na_columns <- setdiff(names(base), by)

  for (j in na_columns) {
    set(base, i = which(is.na(base[, j, with = FALSE])), j=j, value=0)
  }
  #--------------------------------------------------------------------
  return(base)
}


intersect1 <- function(x, y) {
  intersect(x, y)
}

aggregate1 <- function(x, by, value.var) {
  columns <- value.var[value.var %in% names(x)]
  if(length(columns) == 0) {
    return(x[!duplicated(x, by = by), by, with = FALSE])
  } else {
    return(x[, lapply(.SD, sum), by = by, .SDcols = columns])
  }
}

to_data_table <- function(x) {
  if(is.data.table(x)) {
    return(x)
  } else {
    return(data.table(x))
  }
}
