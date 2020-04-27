#' @title sxtRSD
#' @description Calculate RSD.
#' @param x A vector of number.
#' @param rm.na Remove NA or not.
#' @author Xiaotao Shen
#' @export


sxtRSD <- function(x, rm.na = FALSE) {
  a <- regexpr("[a-zA-z]",x)
  if (any(a == 1)) {warning("x has character!!!")}
  if (rm.na) {x <- x[!is.na(x)]}
  x <- as.numeric(x)
    sd(x) / mean(x)
}