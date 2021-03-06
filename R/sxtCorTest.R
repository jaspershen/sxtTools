#' @title sxtCorTest
#' @description sxtCorTest
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param data A maxtrix or data.frame.
#' @return like cor.
#' @export

sxtCorTest <- function(x,
                       alternative = c("two.sided", "less", "greater"),
                       method = c("pearson", "kendall", "spearman"),
                       exact = NULL, conf.level = 0.95, continuity = FALSE,
                       return.type = c("list", "matrix")){

  alternative <- match.arg(alternative)
  method <- match.arg(method)
  return.type <- match.arg(return.type)

  x <- apply(x, 2, list)
  x <- lapply(x, unlist)

  p <- lapply(x, function(y){
    unlist(lapply(x, function(z){
      cor.test(x = z, y = y, alternative = alternative,
               method = method, exact = exact, conf.level = conf.level,
               continuity = continuity)$p.value
    }))
  })

  if(return.type != "list"){
    p <- do.call(rbind, p)
  }

  p
}