SXTerrorbar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}




temp.data1 <- sample(90:100, 10)
temp.data2 <- sample(90:100, 10)



temp <- barplot(c(median(temp.data1), median(temp.data2)), ylim = c(0, 100))

par(xpd = TRUE)
SXTerrorbar(x = temp[1,1], y = median(temp.data1), upper = sd(temp.data1))
SXTerrorbar(x = temp[2,1], y = median(temp.data2), upper = sd(temp.data2))
