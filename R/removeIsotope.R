#' @title removeIsotope
#' @description removeIsotope
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param data A maxtrix or data.frame.
#' @param data.from data from where?
#' @return A data without isotopes.
#' @export

setGeneric(name = "removeIsotope",
           def = function(data,
                          data.from = c("metdna","metanalyzer", "xcms")){
             data.from <- match.arg(data.from)
             if(data.from == "metdna"){
               isotope <- data$isotope
               index <- lapply(isotope, function(x){
                 if(is.na(x)) return(NA)
                 x <- strsplit(x, split = ";")[[1]]
                 temp.idx <- which(x == "[M]")
                 temp.idx
               })

               for(i in 1:length(index)){
                 # cat(i, " ")
                 x <- data[i,,drop = FALSE]

                 if(length(index[[i]]) == 0) {
                   x[1,c(4,5,6,7,8,9,10,11,12,13,14)] <- NA
                   data[i,] <- x
                   next()
                 }

                 if(is.na(index[[i]][1])) next()

                 x[1,4] <- paste(strsplit(x[1,4], split = ";")[[1]][index[[i]]], collapse = ";")
                 x[1,5] <- paste(strsplit(x[1,5], split = ";")[[1]][index[[i]]], collapse = ";")
                 x[1,6] <- paste(strsplit(x[1,6], split = ";")[[1]][index[[i]]], collapse = ";")
                 x[1,7] <- paste(strsplit(x[1,7], split = ";")[[1]][index[[i]]], collapse = ";")
                 x[1,8] <- paste(strsplit(x[1,8], split = ";")[[1]][index[[i]]], collapse = ";")
                 x[1,9] <- paste(strsplit(x[1,9], split = ";")[[1]][index[[i]]], collapse = ";")
                 x[1,10] <- paste(strsplit(x[1,10], split = ";")[[1]][index[[i]]], collapse = ";")
                 x[1,11] <- paste(strsplit(x[1,11], split = ";")[[1]][index[[i]]], collapse = ";")
                 x[1,12] <- paste(strsplit(x[1,12], split = ";")[[1]][index[[i]]], collapse = ";")
                 x[1,13] <- paste(strsplit(x[1,13], split = ";")[[1]][index[[i]]], collapse = ";")
                 x[1,14] <- paste(strsplit(x[1,14], split = ";")[[1]][index[[i]]], collapse = ";")
                 data[i,] <- x
               }

             }
             data <- return(data)
           })