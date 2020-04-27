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

setGeneric(
  name = "getWorklist",
  def = function(table.name = "batch1.xlsx",
                 instrument = c("Thermo", "Agilent", "AB"),
                 each.mode.number = 30,
                 randommethod = c("no", "position", "injection"),
                 samplenumber = NULL,
                 replication = 1,
                 QCstep = 8,
                 conditionQCnumber = 5,
                 qc.index.from = 1,
                 dir = "D:\\Liang\\SmarD_Urine_20190731\\RPLC\\",
                 method.path = "D:\\Liang\\Methods\\RPLC\\Urine\\",
                 ms1.method.pos = "Hypersil_GOLD_MS_pos",
                 ms1.method.neg = "Hypersil_GOLD_MS_neg",
                 ms2.method.pos = c("Hypersil_GOLD_MSMS_pos_NCE25_70_250",
                                    "Hypersil_GOLD_MSMS_pos_NCE25_240_400",
                                    "Hypersil_GOLD_MSMS_pos_NCE25_390_600",
                                    "Hypersil_GOLD_MSMS_pos_NCE25_590_1000",
                                    "Hypersil_GOLD_MSMS_pos_NCE50_70_250",
                                    "Hypersil_GOLD_MSMS_pos_NCE50_240_400",
                                    "Hypersil_GOLD_MSMS_pos_NCE50_390_600",
                                    "Hypersil_GOLD_MSMS_pos_NCE50_590_1000"),
                 ms2.method.neg = c("Hypersil_GOLD_MSMS_neg_NCE25_70_250",
                                    "Hypersil_GOLD_MSMS_neg_NCE25_240_400",
                                    "Hypersil_GOLD_MSMS_neg_NCE25_390_600",
                                    "Hypersil_GOLD_MSMS_neg_NCE25_590_1000",
                                    "Hypersil_GOLD_MSMS_neg_NCE50_70_250",
                                    "Hypersil_GOLD_MSMS_neg_NCE50_240_400",
                                    "Hypersil_GOLD_MSMS_neg_NCE50_390_600",
                                    "Hypersil_GOLD_MSMS_neg_NCE50_590_1000")
                 ) {
    instrument <- match.arg(instrument)
    randommethod <- match.arg(randommethod)
    options(warn = -1)
    file <- dir()
    if (instrument == "Thermo") {
      batch <- readxl::read_excel(table.name)
      if (randommethod == "no") {
        ###add position
        position <-
          unlist(lapply(c("B", "G", "R"), function(y) {
            paste(y,
                  unlist(lapply(LETTERS[1:5], function(x) {
                    paste(x, 1:8, sep = "")
                  })),
                  sep = "")
          }))

        position <- position[-c(1:8)]

        condition.qc <-
          matrix(rep(
            c("Condition.QC", 'Condition.QC', "BA1"),
            conditionQCnumber
          ),
          ncol = 3,
          byrow = TRUE)

        condition.qc <-
          as.data.frame(condition.qc, stringsAsFactors = FALSE)
        colnames(condition.qc) <-
          c("File.Name", "Sample.ID", 'Position')

        blank.qc <-
          data.frame(
            "File.Name" = c("Blank", "QC"),
            "Sample.ID" = c("Blank", "QC"),
            "Position" = c("BA2", "BA1"),
            stringsAsFactors = FALSE
          )

        blank <-
          data.frame(
            "File.Name" = c("Blank", "Blank", "Blank"),
            "Sample.ID" = c("Blank", "Blank", "Blank"),
            "Position" = c("BA2", "BA2", "BA2"),
            stringsAsFactors = FALSE
          )

        dilution.qc <- data.frame(
          "File.Name" = c("DL_QC1_1", "DL_QC2_1", "DL_QC4_1", "DL_QC8_1", "DL_QC1_2", "DL_QC2_2", "DL_QC4_2", "DL_QC8_2"),
          "Sample.ID" = c("DL_QC1_1", "DL_QC2_1", "DL_QC4_1", "DL_QC8_1", "DL_QC1_2", "DL_QC2_2", "DL_QC4_2", "DL_QC8_2"),
          "Position" = c("BA3", "BA4", "BA5", "BA6", "BA3", "BA4", "BA5", "BA6"),
          stringsAsFactors = FALSE
        )

        ms2.qc <-
          data.frame(
            "File.Name" = c("QC_MS2_NCE25_1", "QC_MS2_NCE25_2", "QC_MS2_NCE25_3", "QC_MS2_NCE25_4",
                            "QC_MS2_NCE50_1", "QC_MS2_NCE50_2", "QC_MS2_NCE50_3", "QC_MS2_NCE50_4"),
            "Sample.ID" = c("QC_MS2_NCE25_1", "QC_MS2_NCE25_2", "QC_MS2_NCE25_3", "QC_MS2_NCE25_4",
                            "QC_MS2_NCE50_1", "QC_MS2_NCE50_2", "QC_MS2_NCE50_3", "QC_MS2_NCE50_4"),
            "Position" = c("BA1", "BA1", "BA1", "BA1", "BA1", "BA1", "BA1", "BA1"),
            stringsAsFactors = FALSE
          )

        batch <- cbind(batch, batch)
        batch <- data.frame(batch,
                            "Position" = rep(position, nrow(batch))[1:nrow(batch)],
                            stringsAsFactors = FALSE)
        colnames(batch) <-
          c("File.Name", "Sample.ID", "Position")
        temp.class <- sort(rep(1:nrow(batch), QCstep))
        temp.class <- temp.class[1:nrow(batch)]
        batch <-
          data.frame(temp.class, batch, stringsAsFactors = FALSE)
        batch <-
          plyr::dlply(.data = batch,
                      .variables = plyr::.(temp.class))

        batch <- lapply(batch, function(x) {
          rbind(blank.qc, x[, -1, drop = FALSE])
        })

        batch <- do.call(rbind, batch)
        batch <- rbind(batch, blank.qc)
        rownames(batch) <- NULL

        batch.pos <- data.frame(batch,
                                Path = paste(dir, "POS", sep = ""),
                                Instrument.Method = paste(method.path, ms1.method.pos, sep = ""))

        batch.neg <- data.frame(batch,
                                Path = paste(dir, "NEG", sep = ""),
                                Instrument.Method = paste(method.path, ms1.method.neg, sep = ""))

        batch.pos <-
          batch.pos[, c("File.Name", "Sample.ID", "Path", "Instrument.Method", "Position")]
        batch.neg <-
          batch.neg[, c("File.Name", "Sample.ID", "Path", "Instrument.Method", "Position")]

        condition.qc.pos <- data.frame(
          condition.qc,
          "Path" = paste(dir, 'POS', sep = ""),
          Instrument.Method = paste(method.path, ms1.method.pos, sep = ""),
          stringsAsFactors = FALSE
        )

        condition.qc.neg <- data.frame(
          condition.qc,
          "Path" = paste(dir, 'NEG', sep = ""),
          Instrument.Method = paste(method.path, ms1.method.neg, sep = ""),
          stringsAsFactors = FALSE
        )

        blank.pos <- data.frame(blank,
          "Path" = paste(dir, 'POS', sep = ""),
          Instrument.Method = paste(method.path, ms1.method.pos, sep = ""),
          stringsAsFactors = FALSE
        )

        blank.neg <- data.frame(blank,
          "Path" = paste(dir, 'NEG', sep = ""),
          Instrument.Method = paste(method.path, ms1.method.neg, sep = ""),
          stringsAsFactors = FALSE
        )


        dilution.qc.pos <- data.frame(
          dilution.qc,
          "Path" = paste(dir, 'POS', sep = ""),
          Instrument.Method = paste(method.path, ms1.method.pos, sep = ""),
          stringsAsFactors = FALSE
        )

        dilution.qc.neg <- data.frame(
          dilution.qc,
          "Path" = paste(dir, 'NEG', sep = ""),
          Instrument.Method = paste(method.path, ms1.method.neg, sep = ""),
          stringsAsFactors = FALSE
        )


        ms2.qc.pos <- data.frame(
          ms2.qc,
          "Path" = paste(dir, 'POS', sep = ""),
          Instrument.Method = paste(method.path, ms2.method.pos, sep = ""),
          stringsAsFactors = FALSE
        )

        ms2.qc.neg <- data.frame(
          ms2.qc,
          "Path" = paste(dir, 'NEG', sep = ""),
          Instrument.Method = paste(method.path, ms2.method.neg, sep = ""),
          stringsAsFactors = FALSE
        )

        condition.qc.pos <- condition.qc.pos[, colnames(batch.pos)]
        condition.qc.neg <- condition.qc.neg[, colnames(batch.neg)]

        blank.pos <- blank.pos[, colnames(batch.pos)]
        blank.neg <- blank.neg[, colnames(batch.neg)]

        dilution.qc.pos <- dilution.qc.pos[, colnames(batch.pos)]
        dilution.qc.neg <- dilution.qc.neg[, colnames(batch.neg)]

        ms2.qc.pos <- ms2.qc.pos[, colnames(batch.pos)]
        ms2.qc.neg <- ms2.qc.neg[, colnames(batch.neg)]

        batch.pos <- rbind(condition.qc.pos, dilution.qc.pos, ms2.qc.pos, batch.pos, blank.pos)
        batch.neg <- rbind(condition.qc.neg, dilution.qc.neg, ms2.qc.neg, batch.neg, blank.neg)

        ###rename
        batch.pos[which(batch.pos[, 1] == "Condition.QC"), 1] <-
          paste(batch.pos[which(batch.pos[, 1] == "Condition.QC"), 1],
                1:length(which(batch.pos[, 1] == "Condition.QC")), sep = "_")

        batch.pos[which(batch.pos[, 1] == "QC"), 1] <-
          paste("QC", qc.index.from:(sum(batch.pos[, 1] == "QC") + qc.index.from-1),
                sep = "_")

        batch.pos[which(batch.pos[, 1] == "Blank"), 1] <-
          paste(batch.pos[which(batch.pos[, 1] == "Blank"), 1],
                1:length(which(batch.pos[, 1] == "Blank")), sep = "_")

        batch.pos[, 2] <- batch.pos[, 1]


        batch.neg[which(batch.neg[, 1] == "Condition.QC"), 1] <-
          paste(batch.neg[which(batch.neg[, 1] == "Condition.QC"), 1],
                1:length(which(batch.neg[, 1] == "Condition.QC")), sep = "_")

        batch.neg[which(batch.neg[, 1] == "QC"), 1] <-
          paste("QC", qc.index.from:(sum(batch.neg[, 1] == "QC") + qc.index.from-1),
                sep = "_")

        batch.neg[which(batch.neg[, 1] == "Blank"), 1] <-
          paste(batch.neg[which(batch.neg[, 1] == "Blank"), 1],
                1:length(which(batch.neg[, 1] == "Blank")), sep = "_")

        batch.neg[, 2] <- batch.neg[, 1]

        write.csv(batch.pos, "worklist.pos.csv", row.names = FALSE)
        write.csv(batch.neg, "worklist.neg.csv", row.names = FALSE)


        # each.mode.number <-
        #   ceiling(each.mode.number / QCstep) * (QCstep + 2)
        #
        # temp.class <-
        #   sort(rep(1:nrow(batch.pos), each.mode.number))
        # temp.class <- temp.class[1:nrow(batch.pos)]
        #
        # batch.pos <- data.frame(temp.class, batch.pos,
        #                         stringsAsFactors = FALSE)
        #
        # batch.neg <- data.frame(temp.class, batch.neg,
        #                         stringsAsFactors = FALSE)
        #
        # batch.pos <-
        #   plyr::dlply(.data = batch.pos,
        #               .variables = plyr::.(temp.class))
        #
        # batch.neg <-
        #   plyr::dlply(.data = batch.neg,
        #               .variables = plyr::.(temp.class))
        #
        # batch <- mapply(
        #   FUN = function(x, y) {
        #     list(rbind(x, y))
        #   },
        #   x = batch.pos,
        #   y = batch.neg
        # )
        #
        # batch <- do.call(rbind, batch)
        #
        # rownames(batch) <- NULL
        # batch <- batch[, -1, drop = FALSE]
        #
        # condition.qc <- data.frame(
        #   condition.qc,
        #   "Path" = paste(dir, 'POS', sep = ""),
        #   stringsAsFactors = FALSE
        # )
        #
        # blank <- data.frame(blank,
        #                     "Path" = paste(dir, 'NEG', sep = ""),
        #                     stringsAsFactors = FALSE)
        #
        # condition.qc <- condition.qc[, colnames(batch)]
        # blank <- blank[, colnames(batch)]
        #
        # batch <- rbind(condition.qc, batch, blank)
        #
        #
        # ###rename
        # batch[which(batch[, 1] == "Condition.QC"), 1] <-
        #   paste(batch[which(batch[, 1] == "Condition.QC"), 1],
        #         1:length(which(batch[, 1] == "Condition.QC")), sep = "_")
        #
        # batch[which(batch[, 1] == "QC"), 1] <-
        #   paste(batch[which(batch[, 1] == "QC"), 1],
        #         1:length(which(batch[, 1] == "QC")), sep = "_")
        #
        # batch[which(batch[, 1] == "Blank"), 1] <-
        #   paste(batch[which(batch[, 1] == "Blank"), 1],
        #         1:length(which(batch[, 1] == "Blank")), sep = "_")
        #
        # batch[, 2] <- batch[, 1]
        #
        # write.csv(batch, "worklist.csv", row.names = FALSE)
      }
    }
  }
)
