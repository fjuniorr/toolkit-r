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
install_relatorios <- function(ref = "master", password) {
  devtools::install_bitbucket("dcgf/relatorios", ref = ref , auth_user = "dcgf-admin", password = password)
}

#' @export
install_reest <- function(ref = "master", password) {
  devtools::install_bitbucket("dcgf/reest", ref = ref , auth_user = "dcgf-admin", password = password)
}

#' @export
install_execucao <- function(ref = "master", password) {
  devtools::install_bitbucket("dcgf/execucao", ref = ref , auth_user = "dcgf-admin", password = password)
}

#' @export
install_receita <- function(ref = "master", password) {
  devtools::install_bitbucket("dcgf/receita", ref = ref , auth_user = "dcgf-admin", password = password)
}

#' @export
install_pessoal <- function(ref = "master", password) {
  devtools::install_bitbucket("dcgf/pessoal", ref = ref , auth_user = "dcgf-admin", password = password)
}

#' @export
clean_R_dev <- function() {
  lib <- "~/R-dev"
  pkgs_info <- installed.packages(lib)
  pkgs <- unname(pkgs_info[, 1])
  remove.packages(pkgs, lib)
}
