#' @title errordDisplay
#' @description Displary the error.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param expr An R expression.
#' @param error.info The error information you want to display.
#' @return  Detailed expression information.
#' @export

errorDisplay <- function(expr,
                         error.info = "error"){
  process.result <- try(expr,
                        silent = TRUE)
  if(class(process.result) == "try-error"){
    cat(error.info)
    process.result <- process.result
  }else{
    process.result <- "Right"
  }
}