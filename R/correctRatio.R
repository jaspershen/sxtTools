setGeneric(name = "correctRatio",
           function(x, y, top = 3){
  if(lenght(y) > top) y <- y[1:top]
  if(length(intersect(x, y)) > 0) return("correct")
  return("wrong")
})