#' @title decideCIE
#' @description Decide it is correct, isomer or error identification.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param id1 Standard IDs.
#' @param id2 Identifications from new software.
#' @param top1 Top n for ID1.
#' @param top2 Top n for ID2.
#' @param id.type the ID type, KEGG, HMDB or CAS.
#' @library The library.
#' @return  Correct, isomer and wrong.
#' @export


setGeneric(name = "decideCIE",
           def = function(id1,
                          id2,
                          top1 = 1,
                          top2 = 3,
                          id.type = c("kegg", "hmdb", "cas"),
                          library = NULL){

             if(is.null(library)) stop("You should provide the library which contains the KEGG, HMDB and CAS ID, and Formula\n")

             library <- as.data.frame(library)

             id.type <- match.arg(id.type)
             if(is.na(id1)) stop("id1 should not be NA!")
             if(is.null(id1)) stop("id1 should not be NULL!")
             if(id1=="") stop("id1 should not be NULL!")

             id1 <- strsplit(id1, split = ";")[[1]][1:top1]
             id1 <- id1[!is.na(id1)]

             if(is.na(id2)) return("no")
             if(is.null(id2)) return("no")
             if(id2 == "") return("no")

             id2 <- strsplit(id2, split = ";")[[1]][1:top2]
             id2 <- id2[!is.na(id2)]

             if(length(intersect(id1, id2)) > 0) return("correct")

             temp.idx <- switch(id.type,
               kegg = match("KEGG.ID", colnames(library)),
               hmdb = match("KEGG.ID", colnames(library)),
               cas = match("CAS.ID", colnames(library))
             )

             formula1 <- library$Formula[match(id1, library[,temp.idx])]
             formula2 <- library$Formula[match(id2, library[,temp.idx])]

             if(length(intersect(formula1, formula2)) > 0) return("isomer")
             return('error')
           })