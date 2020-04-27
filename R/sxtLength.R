#' @title sxtLength
#' @description Calculate length of multiple vectors.
#' @param ... vectors.
#' @author Xiaotao Shen
#' @export

sxtLength <- function(...) {
  data <- list(...)
  cat(unlist(lapply(data, length)))
}