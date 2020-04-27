sxtLog <- function(sample, method = c("no", "log2", "loge", "log10")){
  method <- match.arg(method)
  if(method == "no") sample <- sample
  if(method == "log2") sample <- log(sample + 0.00000001, 2)
  if(method == "loge") sample <- log(sample + 0.00000001)
  if(method == "log10") sample <- log(sample + 0.00000001, 10)
  sample

}