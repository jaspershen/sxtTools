#' @title correctRatio
#' @description Calculate top n correction.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param x A vector.
#' @param y A vector.
#' @return  correct or wrong.
#' @export

setGeneric(name = "correctRatio",
           function(x, y, top = 3){
  if(lenght(y) > top) y <- y[1:top]
  if(length(intersect(x, y)) > 0) return("correct")
  return("wrong")
})