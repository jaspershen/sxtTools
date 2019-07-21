
#' @title getWorklist
#' @description Generate Worklist for MS data acquisition.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param instrument Which instrument you use?
#' "Agilent", "AB" or "Thermo", default is "Thermo".
#' @param name The name of worklist.
#' @param randommethod Which random method you want to use?
#' "no", "position" or "injection". Default is "no".
#' @param samplenumber Sample number.
#' @param replication Replication times.
#' @param QCstep QC step.
#' @param conditionQCnumber Condition QC number.
#' @param validationQCstep Validation QC step.
#' @param testmixstep Test mixture step.
#' @param injectionfrom Injection order from which? Default is 1.
#' @param user Default is "other".
#' @param dir Directory.
#' @return New worklist.
#' @export



setGeneric(name = "getWorklist",
           def = function(instrument = c("Thermo", "Agilent", "AB"),
                          polarity = c("both", "positive", "negative"),
                          each.mode.number = 30,
                          name = "worklist",
                          randommethod = c("no", "position", "injection"),
                          samplenumber = NULL,
                          replication = 1,
                          QCstep = 6,
                          conditionQCnumber = 5,
                          validationQCstep = 40,
                          testmixstep = 40,
                          injectionfrom = 1,
                          user = "other",
                          dir = "D:\\Liang\\SMARD-URINE-20190701\\HILIC\\"){


             instrument <- match.arg(instrument)
             randommethod <- match.arg(randommethod)
             #names is the name of the folder,plates is the used plates,
             #if AB,dir is ""
             options(warn = -1)
             file <- dir()

             if(instrument == "Thermo"){
               batch <- readr::read_csv("batch.design.csv")
               if(randommethod == "no"){
                 ###add position
                 position <- unlist(lapply(c("B", "G", "R"), function(y){
                   paste(y,
                         unlist(lapply(LETTERS[1:5], function(x){
                           paste(x, 1:8, sep = "")
                         })),
                         sep = ""
                   )
                 }))

                 position <- position[-c(1:8)]
                condition.qc <- matrix(rep(c("Condition.QC", 'Condition.QC', "BA1"),
                                           conditionQCnumber*2),
                                       ncol = 3, byrow = TRUE)
                condition.qc <- as.data.frame(condition.qc, stringsAsFactors = FALSE)
                colnames(condition.qc) <- c("File.Name", "Sample.ID", 'Position')
                blank.qc <- data.frame("File.Name" = c("Blank", "QC"),
                                       "Sample.ID" = c("Blank", "QC"),
                                       "Position" = c("BA2", "BA1"),
                                       stringsAsFactors = FALSE
                )

                blank <- data.frame("File.Name" = c("Blank", "Blank", "Blank"),
                                    "Sample.ID" = c("Blank", "Blank", "Blank"),
                                    "Position" = c("BA2","BA2","BA2"),
                                    stringsAsFactors = FALSE
                )

                batch <- cbind(batch, batch)
                batch <- data.frame(batch,
                                    "Position" = rep(position, nrow(batch))[1:nrow(batch)],
                                    stringsAsFactors = FALSE)
                colnames(batch) <- c("File.Name", "Sample.ID", "Position")
                temp.class <- sort(rep(1:nrow(batch), QCstep))
                temp.class <- temp.class[1:nrow(batch)]
                batch <- data.frame(temp.class, batch, stringsAsFactors = FALSE)
                batch <- plyr::dlply(.data = batch, .variables = plyr::.(temp.class))

                # if(nrow(tail(batch, 1)[[1]]) < nrow(head(batch, 1)[[1]])/3){
                #
                # }


                batch <- lapply(batch, function(x){
                  rbind(blank.qc, x[,-1,drop = FALSE])
                })


                batch <- do.call(rbind, batch)
                rownames(batch) <- NULL

                # batch <- rbind(condition.qc, batch, blank)

                batch.pos <- data.frame(
                  batch,
                  Path = paste(dir, "POS", sep = ""))

                batch.neg <- data.frame(
                  batch,
                  Path = paste(dir, "NEG", sep = ""))

                batch.pos <- batch.pos[,c("File.Name", "Sample.ID", "Path", "Position")]
                batch.neg <- batch.neg[,c("File.Name", "Sample.ID", "Path", "Position")]

                each.mode.number <- ceiling(each.mode.number / QCstep) * (QCstep + 2)

                temp.class <- sort(rep(1:nrow(batch.pos), each.mode.number))
                temp.class <- temp.class[1:nrow(batch.pos)]

                batch.pos <- data.frame(temp.class, batch.pos,
                                        stringsAsFactors = FALSE)

                batch.neg <- data.frame(temp.class, batch.neg,
                                        stringsAsFactors = FALSE)

                batch.pos <- plyr::dlply(.data = batch.pos, .variables = plyr::.(temp.class))

                batch.neg <- plyr::dlply(.data = batch.neg, .variables = plyr::.(temp.class))

                batch <- mapply(FUN = function(x,y){
                  list(rbind(x, y))
                },
                x = batch.pos,
                y = batch.neg)

                batch <- do.call(rbind, batch)

                rownames(batch) <- NULL
                batch <- batch[,-1,drop = FALSE]

                condition.qc <- data.frame(condition.qc,
                                           "Path" = paste(dir, 'POS', sep = ""),
                                           stringsAsFactors = FALSE)

                blank <- data.frame(blank,
                                    "Path" = paste(dir, 'NEG', sep = ""),
                                    stringsAsFactors = FALSE)

                condition.qc <- condition.qc[, colnames(batch)]
                blank <- blank[, colnames(batch)]

                batch <- rbind(condition.qc, batch, blank)


                ###rename
                batch[which(batch[,1] == "Condition.QC"),1] <-
                  paste(batch[which(batch[,1] == "Condition.QC"),1],
                        1 : length(which(batch[,1] == "Condition.QC")), sep = "_")

                batch[which(batch[,1] == "QC"),1] <-
                  paste(batch[which(batch[,1] == "QC"),1],
                        1 : length(which(batch[,1] == "QC")), sep = "_")

                batch[which(batch[,1] == "Blank"),1] <-
                  paste(batch[which(batch[,1] == "Blank"),1],
                        1 : length(which(batch[,1] == "Blank")), sep = "_")

                batch[,2] <- batch[,1]

                write.csv(batch, "worklist.csv", row.names = FALSE)





               }
             }else{

               if (instrument == "AB") {dir = ""}
               x <- read.csv(file[file == "batch.design.csv"],
                             check.names = FALSE,
                             stringsAsFactors = FALSE)

               # --------------------------------------------------------------------------
               options(warn = 0)
               x <- as.character(x[, 1])
               na.number <- sum(is.na(x))
               x <- x[!is.na(x)]
               space.number <- sum(x == "")
               x <- x[x != ""]

               cat(
                 paste(
                   "\nThere are",
                   na.number,
                   "NAs and",
                   space.number,
                   "spaces in your batch design, please confirm there are no error.\n"
                 )
               )

               # ---------------------------------------------------------------------------
               if (is.null(samplenumber)) {samplenumber <- length(x)} else {
                 if (samplenumber > length(x)) {
                   samplenumber <- samplenumber
                   warning("The sample number you set is larger than
                         the sample in your batch design.\n")
                 } else {
                   samplenumber <- samplenumber
                 }

               }
               x <- x[1:samplenumber]
               # ------------------------------------------------------------------------
               options(warn = -1)
               plate1 <- rep(1, 51)
               plate2 <- rep(2, 51)
               plate3 <- rep(3, 51)
               plate4 <- rep(4, 51)
               plate5 <- rep(5, 51)
               plate6 <- rep(6, 51)

               plate <- rep(c(plate1, plate2), 6)
               plate <- plate[1:(samplenumber * replication)]
               real.plate <-
                 rep(c(plate1, plate2, plate3, plate4, plate5, plate6), 6)
               real.plate <- real.plate[1:(samplenumber * replication)]
               vial.position <- rep(c(1:51), 6)
               vial.position <- vial.position[1:(samplenumber * replication)]

               sub.position <- list()
               for (i in 1:6) {
                 sub.position[[i]] <- paste(LETTERS[i], c(1:9), sep = "")
               }
               sub.position <- unlist(sub.position)

               real.position <- list()
               for (i in 1:12) {
                 real.position[[i]] <-
                   paste(paste("P", i, sep = ""), sub.position, sep = "-")
               }

               real.position <- unlist(real.position)

               position <- rep(real.position[1:108], 6)

               position <- position[1:(samplenumber * replication)]
               real.position <- real.position[1:(samplenumber * replication)]
               sub.position96 <-
                 c(
                   paste("A", c(1:12), sep = ""),
                   paste("B", c(1:12), sep = ""),
                   paste("C", c(1:12), sep = ""),
                   paste("D", c(1:12), sep = ""),
                   paste("E", c(1:12), sep = ""),
                   paste("F", c(1:12), sep = ""),
                   paste("G", c(1:12), sep = ""),
                   paste("H", c(1:12), sep = "")
                 )
               real.position96 <-
                 rep(c(
                   paste("P1", sub.position96, sep = "-"),
                   paste("P2", sub.position96, sep = "-"),
                   paste("P3", sub.position96, sep = "-"),
                   paste("P4", sub.position96, sep = "-"),
                   paste("P5", sub.position96, sep = "-"),
                   paste("P6", sub.position96, sep = "-")
                 ), 4)
               real.position96 <- real.position96[1:(samplenumber * replication)]

               position96 <-
                 rep(c(
                   paste("P1", sub.position96, sep = "-"),
                   paste("P2", sub.position96, sep = "-")
                 ), 8)
               position96 <- position96[1:(samplenumber * replication)]

               # -------------------------------------------------------------------------

               ###repeat samples?
               if (replication == 1) {
                 x <- x
               } else{
                 x2 <- NULL
                 for (i in seq_len(replication)) {
                   x1 <- paste(x, i, sep = ".")
                   x2 <- cbind(x2, x1)
                 }
                 x <- x2
               }

               x <- as.character(x)
               #random position or random injection order or no
               if (instrument == "Agilent") {
                 if (randommethod == "position") {
                   random.order <- sample(1:(samplenumber * replication))
                   x <- data.frame(random.order, x)
                   x <- x[order(as.numeric(x[, 1])),]
                   x <- as.character(x[, -1])
                   x <-
                     cbind(x, position, real.position, position96, real.position96)
                   colnames(x) <-
                     c(
                       "Sample Name",
                       "Position in 54 plate",
                       "Real position in 54 plate",
                       "Position in 96 plate",
                       "Real position in 96 plate"
                     )
                   write.csv(x, sprintf("%s sample info.csv", name))
                   x <- x[, -c(3, 4, 5)]
                 }
                 if (randommethod == "injection") {
                   if (length(x) > 108)
                   {
                     warning("The sample number is larger than 108,
                           injection order random is not commended.")
                   }
                   x <-
                     cbind(x, position, real.position, position96, real.position96)
                   colnames(x) <-
                     c(
                       "Sample Name",
                       "Position in 54 plate",
                       "Real position in 54 plate",
                       "Position in 96 plate",
                       "Real position in 96 plate"
                     )
                   write.csv(x, sprintf("%s sample info.csv", name))
                   random.order <- sample(1:(samplenumber * replication))
                   x <- data.frame(random.order, x)
                   x <- x[order(x[, 1]),]
                   x <- x[, -c(1, 4, 5, 6)]
                 }
                 if (randommethod == "no") {
                   x <- cbind(x, position, real.position, position96, real.position96)
                   colnames(x) <-
                     c(
                       "Sample Name",
                       "Position in 54 plate",
                       "Real position in 54 plate",
                       "Position in 96 plate",
                       "Real position in 96 plate"
                     )
                   write.csv(x, sprintf("%s sample info.csv", name))
                   x <- x[, -c(3, 4, 5)]
                 }
               }
               ##AB instrument
               if (instrument == "AB") {
                 if (randommethod == "position") {
                   random.order <- sample(1:(samplenumber * replication))
                   x <- data.frame(random.order, x)
                   x <- x[order(as.numeric(x[, 1])),]
                   x <- x[, -1]
                   x <-
                     cbind(x,
                           plate,
                           vial.position,
                           real.plate,
                           position96,
                           real.position96)
                   colnames(x) <-
                     c(
                       "Sample Name",
                       "Plate",
                       "Position in 54 plate",
                       "Real plate of 54 plate",
                       "Position in 96 plate",
                       "Real position 96 plate"
                     )
                   write.csv(x, "sample info.csv")
                   x <- x[, -c(4, 5, 6)]
                 }

                 if (randommethod == "injection") {
                   x <-
                     cbind(x,
                           plate,
                           vial.position,
                           real.plate,
                           position96,
                           real.position96)
                   colnames(x) <-
                     c(
                       "Sample Name",
                       "Plate",
                       "Position in 54 plate",
                       "Real Plate of 54 plate",
                       "Position in 96 plate",
                       "Real position in 96 plate"
                     )
                   write.csv(x, "sample info.csv")
                   random.order <- sample(1:(samplenumber * replication))
                   x <- cbind(random.order, x)
                   x <- x[order(as.numeric(x[, 1])), ]
                   x <- x[, -c(1, 5, 6, 7)]
                 }

                 if (randommethod == "no") {
                   x <-
                     data.frame(x,
                                plate,
                                vial.position,
                                real.plate,
                                position96,
                                real.position96)
                   colnames(x) <-
                     c(
                       "Sample Name",
                       "Plate",
                       "Position in 54 plate",
                       "Real Plate of 54 plate",
                       "Position in 96 plate",
                       "Real position in 96 plate"
                     )
                   write.csv(x, "sample info.csv")
                   x <- x[, -c(4, 5, 6)]
                 }
               }
               #now x column 1 is Sample Name, column 2 is Sample Position
               if (instrument == "Agilent") {
                 Blank <- c("Blank", "Vial1")
                 Test.mix <- matrix(c("Test.mix", "Vial2"), ncol = 2)
                 validationQC <- matrix(c("validationQC", "Vial3"), ncol = 2)
                 QC <- c("QC", "Vial3")
                 Blank.QC <- rbind(Blank, QC)
               } else {
                 Blank <- c("Blank", "1", "52")
                 Test.mix <- matrix(c("Test.mix", "1", "53"), ncol = 3)
                 validationQC <- matrix(c("validationQC", "1", "54"), ncol = 3)
                 QC <- c("QC", "1", "54")
                 Blank.QC <- rbind(Blank, QC)
               }


               #insert Blank and QC
               x <-
                 lapply(seq(1, nrow(x), by = QCstep), function(y)
                   if (y + QCstep - 1 <= (samplenumber * replication)) {
                     x[y:(y + QCstep - 1),]
                   }
                   else {
                     x[y:nrow(x),]
                   })
               colnames(Blank.QC) <- colnames(x[[1]])
               x <- lapply(x, function(y)
                 rbind(Blank.QC, y))
               ###
               x2 <- NULL
               for (i in seq_along(x)) {
                 x1 <- x[[i]]
                 x2 <- rbind(x2, x1)
               }
               x <- x2
               x <- x[-1, ]

               x <- rbind(x, Blank.QC)
               x <- x[-(nrow(x) - 1), ]

               #insert Test.mix
               if (testmixstep == 0) {
                 x = x
               } else {
                 x <-
                   lapply(seq(1, nrow(x), by = testmixstep), function(y)
                     if (y + testmixstep - 1 <= nrow(x)) {
                       x[y:(y + testmixstep - 1),]
                     } else {
                       x[y:nrow(x),]
                     })


                 colnames(Test.mix) <- colnames(x[[1]])
                 x <- lapply(x, function(y)
                   rbind(Test.mix, y))

                 x3 <- NULL
                 for (i in seq_along(x)) {
                   x1 <- x[[i]]
                   x3 <- rbind(x3, x1)
                 }
                 x <- x3
                 x <- rbind(x, Test.mix)
               }




               #insert validation QC
               if (validationQCstep == 0) {
                 x = x
               } else {
                 x <-
                   lapply(seq(1, nrow(x), by = validationQCstep), function(y)
                     if (y + validationQCstep - 1 <= nrow(x)) {
                       x[y:(y + validationQCstep - 1),]
                     } else {
                       x[y:nrow(x),]
                     })

                 colnames(validationQC) <- colnames(x[[1]])
                 x <- lapply(x, function(y)
                   rbind(validationQC, y))

                 x3 <- NULL
                 for (i in seq_along(x)) {
                   x1 <- x[[i]]
                   x3 <- rbind(x3, x1)
                 }
                 x <- x3
                 x <- rbind(x, validationQC)
               }







               if (instrument == "Agilent") {
                 colnames(x) <- c('Sample.Name', "Sample.Position")
                 x[, 1] <- as.character(x[, 1])
                 x[, 2] <- as.character(x[, 2])
               }
               if (instrument == "AB") {
                 colnames(x) <- c('Sample.Name', "Plate.Position", "Vial.Postion")
                 x[, 1] <- as.character(x[, 1])
                 x[, 2] <- as.character(x[, 2])
                 x[, 3] <- as.character(x[, 3])
               }

               if (instrument == "Agilent") {
                 temp1 <- matrix(rep(Blank, 3), ncol = 2, byrow = TRUE)
                 temp2 <- matrix(rep(QC, conditionQCnumber),
                                 ncol = 2,
                                 byrow = TRUE)
                 temp3 <- matrix(rep(Blank, 3), ncol = 2, byrow = TRUE)
                 colnames(temp1) <-
                   colnames(temp2) <- colnames(temp3) <- colnames(x)
                 x <- rbind(temp1, temp2, x, temp3)
                 x[, 1] <- as.character(x[, 1])
                 x[, 2] <- as.character(x[, 2])
               }
               if (instrument == "AB") {
                 temp1 <- matrix(rep(Blank, 3), ncol = 3, byrow = TRUE)
                 temp2 <- matrix(rep(QC, conditionQCnumber),
                                 ncol = 3,
                                 byrow = TRUE)
                 temp3 <- matrix(rep(Blank, 3), ncol = 3, byrow = TRUE)
                 colnames(temp1) <-
                   colnames(temp2) <- colnames(temp3) <- colnames(x)
                 x <- rbind(temp1, temp2, x, temp3)
                 x[, 1] <- as.character(x[, 1])
                 x[, 2] <- as.character(x[, 2])
                 x[, 3] <- as.character(x[, 3])
               }

               Blank.number <- length(grep("Blank", x[, 1]))
               x[, 1][grep("Blank", x[, 1])] <-
                 paste("Blank", c(1:Blank.number), sep = "")

               Test.mix.number <- length(grep("Test.mix", x[, 1]))
               x[, 1][grep("Test.mix", x[, 1])] <-
                 paste("Test.mix", c(1:Test.mix.number), sep = "")


               validationQC.number <- length(grep("validationQC", x[, 1]))
               x[, 1][grep("validationQC", x[, 1])] <-
                 paste("validationQC", c(1:validationQC.number), sep = "")


               ##

               QC.number <- sum(x[,1] == "QC")

               x[, 1][which(x[,1] == "QC")][1:conditionQCnumber] <-
                 paste("ConditionQC", c(1:conditionQCnumber), sep = "")

               x[, 1][which(x[,1] == "QC")] <-
                 paste("QC", c(1:(length(which(x[,1]=="QC")))), sep = "")

               first <- which(x[, 1] == "QC1")

               last <-
                 which(x[, 1] == sprintf("QC%s", QC.number - conditionQCnumber))

               before.info <- x[1:(first - 1),]
               Data.File1 <- before.info[, 1]

               after.info <- x[(last + 1):nrow(x),]
               Data.File6 <- after.info[, 1]

               middle.info <- x[first:last, ]
               middle.info <- cbind(middle.info, c(1:nrow(middle.info)))
               Sample.QC <-
                 middle.info[setdiff(1:nrow(middle.info),grep("Blank", middle.info[, 1])), ]

               #remove Blank in middle.info

               Sample.QC <-
                 Sample.QC[setdiff(1:nrow(Sample.QC), c(grep("Test.mix", Sample.QC[, 1]),
                                                        grep("validationQC", Sample.QC[, 1]))), ]
               #remove Test.mix and validationQC in middle.info

               middle.blank <-
                 middle.info[grep("Blank", middle.info[, 1]), , drop = FALSE]

               #column 3 is number
               middle.testmix <-
                 middle.info[grep("Test.mix", middle.info[, 1]), , drop = FALSE]

               middle.validationQC <-
                 middle.info[grep("validationQC", middle.info[, 1]), , drop = FALSE]

               Data.File2 <-
                 paste("Sample", c(injectionfrom:(nrow(Sample.QC) + injectionfrom - 1)),
                       sep = "")

               Data.File2 <- paste(Data.File2, Sample.QC[, 1], sep = ".")

               if (user == "other") {
                 Data.File2 <- Sample.QC[, 1]
               }

               if (instrument == "Agilent") {
                 Data.File2 <- cbind(Data.File2, Sample.QC[, 3])
                 Data.File3 <- middle.blank[, c(1, 3)]
                 Data.File4 <- middle.testmix[, c(1, 3)]
                 Data.File5 <- middle.validationQC[, c(1, 3)]
               }
               if (instrument == "AB") {
                 Data.File2 <- cbind(Data.File2, Sample.QC[, 4])
                 Data.File3 <- middle.blank[, c(1, 4)]
                 Data.File4 <- middle.testmix[, c(1, 4)]
                 Data.File5 <- middle.validationQC[, c(1, 4)]
               }

               colnames(Data.File5) <- colnames(Data.File4) <- colnames(Data.File3) <-
                 colnames(Data.File2) <- paste("test", c(1:ncol(Data.File2)))
               Data.File2 <- rbind(Data.File2, Data.File3, Data.File4, Data.File5, Data.File6)
               Data.File2 <- Data.File2[order(as.numeric(Data.File2[, 2])),]
               Data.File2 <- Data.File2[, -2]

               Data.File <- c(Data.File1, as.character(Data.File2), Data.File5)
               name.POS <- paste(name, "POS")
               name.NEG <- paste(name, "NEG")

               #
               Data.File <- x


               Data.File.POS <- paste(dir, name.POS, "\\", Data.File, sep = "")
               Data.File.NEG <- paste(dir, name.NEG, "\\", Data.File, sep = "")
               Data.File.POS <- paste(Data.File.POS, "POS", sep = ".")
               Data.File.NEG <- paste(Data.File.NEG, "NEG", sep = ".")

               if (instrument == "Agilent") {
                 Data.File.POS <- paste(Data.File.POS, "d", sep = ".")
                 Data.File.NEG <- paste(Data.File.NEG, "d", sep = ".")
               }

               x.POS <- cbind(x, Data.File.POS)
               x.NEG <- cbind(x, Data.File.NEG)

               write.csv(x.POS, sprintf("%s POS.csv", name), row.names = FALSE)
               write.csv(x.NEG, sprintf("%s NEG.csv", name), row.names = FALSE)
               cat("Worklist is generated.\n")
             }


             # return(TRUE)

           })



