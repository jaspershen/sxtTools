####heatmap function
setGeneric(name = "heatMap",
           def = function(sample,
                          sample.info,
                          group,
                          color = c("dodgerblue", "firebrick1", "orange",
                                    "springgreen2", "orchid"),
                          int.col = c("navy", "white", "firebrick"),
                          show_rownames = TRUE,
                          show_colnames = FALSE,
                          border_color = NA,
                          fontsize = 10,
                          fontsize_row = 10,
                          fontsize_col = 10,
                          cluster_rows = TRUE,
                          cluster_cols = TRUE,
                          clustering_distance_rows = "euclidean",
                          clustering_distance_cols = "euclidean",
                          clustering_method = "complete",
                          display_numbers = FALSE,
                          legend = TRUE
           ){
             sample.info <- as.data.frame(sample.info)

             # group.idx <- lapply(group, function(x){
             #   which(sample.info$group == x)
             # })

             sample.range <- abs(range(sample))
             dif <- sample.range[1] - sample.range[2]
             if (dif < 0) {
               sample[sample > sample.range[1]] <- sample.range[1]
             }
             if (dif > 0) {
               sample[sample < -1 * sample.range[2]] <- -1 * sample.range[2]
             }

             annotation_col <- data.frame(Group = c(sample.info[,"group"]), stringsAsFactors = FALSE)


             rownames(annotation_col) <- sample.info[,1]


             # Specify colors
             # ann_col <- NULL
             # for (i in seq_along(group)) {
             #   ann_col[i] <- color[i]
             # }

             # ann_col <- c("green", "red")

             # ann_colors = list(group = c("green", "red"))

             ann_colors = list(Group = color[1:length(group)])

             names(ann_colors[[1]]) <- group

             temp <- pheatmap::pheatmap(mat = sample,
                                        color = colorRampPalette(int.col)(50),
                                        annotation_col = annotation_col,
                                        annotation_colors = ann_colors,
                                        show_rownames = show_rownames,
                                        show_colnames = show_colnames,
                                        border_color = border_color,
                                        fontsize = fontsize,
                                        fontsize_row = fontsize_row,
                                        fontsize_col = fontsize_col,
                                        cluster_rows = cluster_rows,
                                        cluster_cols = cluster_cols,
                                        clustering_distance_rows = clustering_distance_rows,
                                        clustering_distance_cols = clustering_distance_cols,
                                        clustering_method = clustering_method,
                                        display_numbers = display_numbers,
                                        legend = legend, silent = TRUE)
             temp
           })