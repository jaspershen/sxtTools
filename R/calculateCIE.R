

setGeneric(name = "calculateCIE",
           function(x,
                    type = c("annotation.rate",
                             "correct",
                             "isomer", "error")){

             type <- match.arg(type)

             if(type == "annotation.rate"){
               return(sum(x != "no")/length(x))
             }

             if(type == "correct"){
               return(sum(x == "correct")/sum(x != "no")  )
             }

             if(type == "isomer"){
               return(sum(x == "isomer")/sum(x != "no")  )
             }

             if(type == "error"){
               return(sum(x == "error")/sum(x != "no")  )
             }
}
)