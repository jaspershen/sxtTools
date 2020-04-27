#' @title openwd
#' @description Open currect work directory.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @return inder.
#' @export

openwd <- function(){
  system(sprintf('open %s', shQuote(getwd())))
}

# setGeneric(name = "openwd",
#            function(){
#              sprintf('open %s', shQuote(getwd()))
#            })