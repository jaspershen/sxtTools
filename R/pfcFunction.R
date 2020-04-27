# # data1 <- readr::read_csv("neg ms1.csv")
# # colnames(data1)
# # sample1 <- data1[,c(13:184)]
# # sample1 <- sample1[,grep("QC", colnames(sample1))]
# # remove.idx <- which(apply(sample1, 1, function(x) sum(x == 0)/ncol(sample1)) > 0.5)
# # if(length(remove.idx) > 0){
# #   sample1 <- sample1[-remove.idx,]
# # }
# #
# # colnames(sample1) <- gsub(pattern = "QC|neg", replacement = "", x = colnames(sample1))
# #
# # pca.object1 <- sxtPCA(sample = sample1, log.method = "log10", scale.method = "auto", center = TRUE)
# # class.info <- rep("QC", ncol(sample1))
# # # class.info[grep("QC", colnames(sample1))] <- "QC"
# # # class.info[is.na(class.info)] <- "Subject"
# # plot1 <- pcaScorePlot(pca.object = pca.object1, class.info = class.info, dimension = 1)
# # plotly::ggplotly(plot1)
# # plot1
# #
# # pc <- pca.object1$x
# # sd(pc[,1])
# #
# #
# # data2 <- readr::read_csv("neg-after.csv")
# # colnames(data2)
# # sample2 <- data2[,c(31:202)]
# # sample2 <- sample2[,grep("QC", colnames(sample2))]
# # colnames(sample2)
# # remove.idx <- which(apply(sample2, 1, function(x) sum(x == 0)/ncol(sample2)) > 0.5)
# # if(length(remove.idx) > 0){
# #   sample2 <- sample2[-remove.idx,]
# # }
# #
# # colnames(sample2) <- gsub(pattern = "QC|neg", replacement = "", x = colnames(sample2))
# # colnames(sample2)
# # pca.object2 <- sxtPCA(sample = sample2, log.method = "log10", scale.method = "auto", center = TRUE)
# # class.info <- rep("QC", ncol(sample2))
# # # class.info[grep("QC", colnames(sample2))] <- "QC"
# # # class.info[is.na(class.info)] <- "Subject"
# # plot2 <- pcaScorePlot(pca.object = pca.object2, class.info = class.info, dimension = 1)
# # plotly::ggplotly(plot2)
# # plot2
# #
# # pc <- pca.object2$x
# # sd(pc[,1])
# #
# #
# #
# # sxtPCA <- function(sample,
# #                    log.method = c("no", "log2", "loge", "log10"),
# #                    scale.method = c("no", "pareto", "auto"),
# #                    center = TRUE){
# #   sample <- as.data.frame(sample)
# #   log.method <- match.arg(log.method)
# #   scale.method <- match.arg(scale.method)
# #   sample <- apply(sample, 2, function(x){
# #     x[is.na(x)] <- 0
# #     x
# #   })
# #   sample <- as.data.frame(sample)
# #   ##log
# #   sample <- sxtLog(sample = sample, method = log.method)
# #   ##scale
# #   sample <- sxtScale(sample = sample,
# #                      method = scale.method,
# #                      center = center)
# #   ##pca
# #   pca.object <- prcomp(data.frame(t(sample)),
# #                        retx = TRUE,
# #                        center = FALSE,
# #                        scale = FALSE)
# #   pca.object
# # }
# #
# #
# #
# #
# # pcaScorePlot <- function(pca.object,
# #                          class.info,
# #                          dimension = 2){
# #
# #   if(is.null(pca.object)) return(NULL)
# #   #group.info has tow column, column 1 is sample name, column is group
# #   pov <- summary(pca.object)$importance[2,]
# #   sd <- summary(pca.object)$importance[1,]
# #   cp <- summary(pca.object)$importance[3,]
# #   pc <- pca.object$x
# #
# #   pc1 <- round(pov[1], 2)
# #   pc2 <- round(pov[2], 2)
# #
# #   data <- data.frame(rownames(pc), pc[,c(1:2)], stringsAsFactors = FALSE)
# #
# #   colnames(data)[1] <- "sample.name"
# #   # data <- data[order(data$sample.name),]
# #
# #   # class.info <- class.info[order(class.info$sample.name),]
# #
# #   data <- data.frame(data, class.info, stringsAsFactors = FALSE)
# #   colnames(data)[ncol(data)] <- "Class"
# #
# #   my.theme <- ggplot2::theme_bw()+
# #     ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
# #                    axis.title.y = ggplot2::element_text(size = 18)) +
# #     ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
# #                    axis.text.y = ggplot2::element_text(size = 15)) +
# #     ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
# #     ggplot2::theme(legend.text = ggplot2::element_text(size = 10))
# #
# #   if(dimension == 1){
# #     p <- ggplot2::ggplot(data = data,
# #                          ggplot2::aes(x = sample.name,
# #                                       y = PC1,
# #                                       colour = Class,
# #                                       # shape = Class,
# #                                       label = sample.name)) +
# #       ggplot2::geom_point(alpha = 0.7)+
# #       ggplot2::ylim(-200, 200)+
# #       geom_hline(yintercept = c(-2*sd(pc[,1]),2*sd(pc[,1])))
# #   }else{
# #     p <- ggplot2::ggplot(data = data,
# #                     ggplot2::aes(x = PC1, y = PC2,
# #                                  colour = Class,
# #                                  # shape = Class,
# #                                  label = sample.name)) +
# #       ggplot2::geom_point(alpha = 0.7) +
# #       ggplot2::labs(x = paste("PC1:", pc1),
# #                     y = paste("PC2", pc2)
# #                     # colour = "Batch"
# #       )
# #   }
# #
# #
# #   score.plot <-
# #     p +
# #     ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 20))+
# #     my.theme+ggplot2::ggtitle("PCA score plot")+
# #     ggplot2::geom_hline(yintercept = 0)+
# #     ggplot2::geom_vline(xintercept = 0)
# #   score.plot
# # }
#
#
#
# 19*3/288
# 8*3/288
# 119*3/288
# 56*3/288
# 52*3/288
# 12*3/288
# 22*3/288
