#' @title installBioc
#' @description Install packages from Bioconductor.
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param package The package name from Bioconductor.
#' @param ... Other parameters. See ?biocLite
#' @author Xiaotao Shen
#' @export

setGeneric(name = "installBioc",
           def = function(package = "Rdisop",
                          web = c("https", "http"),
                          ...){
             web <- match.arg(web)
             if(web == "https"){
               source("https://bioconductor.org/biocLite.R")
             }else{
               source("http://bioconductor.org/biocLite.R")
             }
             biocLite(package, ...)
           })