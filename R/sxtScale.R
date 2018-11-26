sxtScale <- function(sample,
                     method = c("no", "pareto", "auto"),
                     center = TRUE){
  method <- match.arg(method)

  if(method == 'pareto') {
    if(center == TRUE) sample <- t(apply(sample, 1, function(x) (x-mean(x))/sqrt(sd(x))))
    if(center == FALSE) sample <- t(apply(sample, 1, function(x) x/sqrt(sd(x))))
  }

  if(method == 'auto') {
    if(center == TRUE) sample <- t(apply(sample, 1, function(x) (x-mean(x))/sd(x)))
    if(center == FALSE) sample <- t(apply(sample, 1, function(x) x/sd(x)))
  }

  if(method == 'no') {
    sample <- sample
  }

  sample

}