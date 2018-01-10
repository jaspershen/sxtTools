#' @title sxtSetwd
#' @description sxtSetwd.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @export

sxtSetwd <- function() {
  x <- readline("Please paste the directory:")
  setwd(x)
}