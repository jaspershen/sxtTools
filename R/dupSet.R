#' @title dupSet
#' @description dupSet.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param data A maxtrix or data.frame.
#' @return like duplicated.
#' @export

dupSet <- function(data){
  data <- apply(data, 1, list)
  data <- lapply(data, unlist)

  return.result <- NULL
  return.result[1] <- FALSE
  for(k in 2:length(data)){
  pd <- unlist(lapply(data[1:(k-1)], function(y){
    setequal(y, data[[k]])
  }))
  if(any(pd)) {
    return.result[k] <- TRUE
  }else{
    return.result[k] <- FALSE
  }
  }

  return.result
}


# data <- cbind(c("A", "B", "D"), c("D", "E", "A"))
#
# dupSet(data)
