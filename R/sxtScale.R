#' @title sxtScale
#' @description Scale data (matirx or data frame)
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param data A matarix or data frame, rows represents variables, and columns represents observations.
#' @param method Scale method. no, pareto or auto.
#' @param center Center data or not, default is TRUE.
#' @return A scaled data.
#' @export

sxtScale <- function(data,
                     method = c("no", "pareto", "auto"),
                     center = TRUE){
  method <- match.arg(method)

  if(method == 'pareto') {
    if(center == TRUE) data <- t(apply(data, 1, function(x) (x-mean(x))/sqrt(sd(x))))
    if(center == FALSE) data <- t(apply(data, 1, function(x) x/sqrt(sd(x))))
  }

  if(method == 'auto') {
    if(center == TRUE) data <- t(apply(data, 1, function(x) (x-mean(x))/sd(x)))
    if(center == FALSE) data <- t(apply(data, 1, function(x) x/sd(x)))
  }

  if(method == 'no') {
    data <- data
  }

  data

}