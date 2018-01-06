#' @title sxtSection
#' @description Sect vector.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param vector A vector.
#' @param n Length.
#' @return  Sected vector.
#' @export



setGeneric(name = "sxtSection",
           function(vector, n = 1){
if(n > length(vector)) n <- length(vector)
position <- c(1:length(vector))%%n
position[position==0] <- n

idx <- vector(mode = "list", n)
for(i in 1:n){
idx[[i]] <- vector[position == i]
}

each.len <- unlist(lapply(idx, length))
idx1 <- cumsum(each.len)
idx2 <- c(1, idx1[-length(idx1)] + 1)

idx <- cbind(idx2, idx1)

idx <- apply(idx, 1, list)
idx <- lapply(idx, unlist)

lapply(idx, function(x){
  vector[x[1]:x[2]]
})

           })