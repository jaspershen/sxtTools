whichMin <- function(vector, number = 1,
                     return = c("index", "value")){
  return <- match.arg(return)
  vector <- as.numeric(vector)
  if(return == "index"){
    idx <- order(vector, decreasing = FALSE)[1:number]
    idx <- idx[which(!is.na(idx))]
    return(idx)
  }else{
    value <- sort(vector, decreasing = FALSE)[1:number]
    value <- value[which(!is.na(value))]
    return(value)
  }
}



whichMax <- function(vector, number = 1,
                     return = c("index", "value")){
  return <- match.arg(return)
  vector <- as.numeric(vector)
  if(return == "index"){
    idx <- order(vector, decreasing = TRUE)[1:number]
    idx <- idx[which(!is.na(idx))]
    return(idx)
  }else{
    value <- sort(vector, decreasing = TRUE)[1:number]
    value <- value[which(!is.na(value))]
    return(value)
  }
}