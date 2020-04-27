#' @title sxtSetwd
#' @description sxtSetwd.
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @export

sxtSetwd <- function() {
  x <- readline("Please paste the directory:")
  setwd(x)
}