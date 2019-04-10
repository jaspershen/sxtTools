#' @title sxtVolcanoPlot
#' @description Volcano plot
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param p.value A numeric vector for p values.
#' @param fc A numeric vector for fold change values.
#' @param p.cutoff The cutoff of p values.
#' @param fc.cutoff The cutoff of fold change values, only set the larger side.
#' @param linetype The line type for cutoff of p values and fold change values.
#' @param xlab Title of x axis.
#' @param ylab Title of y axis.
#' @param title Title of plot.
#' @param legend.title Legend title.
#' @param legend.label Legend label.
#' @param point.color Colors of points.
#' @param alpha Alpha of points.
#' @param lab.size Font size of axis titles.
#' @param axis.text.size Font size of axis text.
#' @param title.size Font size of title.
#' @param legend.title.size Font size of legend title.
#' @param legend.text.size Font size of axis text.
#' @return A ggplot object.
#' @example
#' @export

sxtVolcanoPlot <- function(p.value,
                           fc,
                           p.cutoff = 0.05,
                           fc.cutoff = 2,
                           linetype = c("3", "2", "1"),
                           xlab = "log2(Fold change)",
                           ylab = "-log10(P-value, FDR)",
                           title = "Volcano plot",
                           legend.title = "Marker",
                           legend.label = c("Yes", "No"),
                           point.color = c("#F8766D", "#619CFF"),
                           alpha = 0.9,
                           lab.size = 12,
                           axis.text.size = 12,
                           title.size = 15,
                           legend.title.size = 12,
                           legend.text.size = 10,
                           ...){

  linetype <- match.arg(linetype)
  linetype <- as.numeric(linetype)
  diff <- rep(NA, length(p.value))
  diff[p.value < p.cutoff & fc > fc.cutoff] <- "yes"
  diff[p.value < p.cutoff & fc < 1/fc.cutoff] <- "yes"
  diff[is.na(diff)] <- "no"
  diff <- factor(diff, levels = c("yes", "no"))

  temp.data <- tibble::tibble(p.value = p.value, fc = fc, diff = diff)

  ##volcano plot
  volcano.plot <- ggplot2::ggplot(data = temp.data) +
    ggplot2::geom_point(mapping = ggplot2::aes(y = -log(p.value, 10), x = log(fc, 2),
                                               color = diff), alpha = alpha) +
    ##add lines in plot
    ggplot2::geom_hline(yintercept = -log(p.cutoff, 10), linetype = linetype) +
    ggplot2::geom_vline(xintercept = log(fc.cutoff, 2), linetype = linetype) +
    ggplot2::geom_vline(xintercept = log(1/fc.cutoff, 2), linetype = linetype) +
    ##xlab and ylab
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    #legend
    # scale_color_discrete(name = legend.title, labels = legend.label) +
    scale_color_manual(name = legend.title, labels = legend.label,
                       values = point.color) +
    theme_bw() +
    #theme
    ggplot2::theme(
      plot.title = ggplot2::element_text(color="black", size = title.size,
                                         face = "plain",
                                         hjust = 0.5),
      axis.title = ggplot2::element_text(color="black", size = lab.size,
                                           face = "plain"),
      axis.text = ggplot2::element_text(color="black", size = axis.text.size,
                                           face = "plain"),
      legend.title = ggplot2::element_text(color="black", size = legend.title.size,
                                           face = "plain"),
      legend.text = ggplot2::element_text(color="black", size = legend.text.size,
                                           face = "plain")
    )

  return(volcano.plot)
}