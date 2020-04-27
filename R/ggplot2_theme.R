#' @title theme_sxtlight
#' @description Calculate top n correction.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param x A vector.
#' @param y A vector.
#' @return  correct or wrong.
#' @export


theme_sxtlight <- function(...) {
  require(grid)
  ggplot2::theme_bw(...) +
    ggplot2::theme(
      axis.title.x = element_text(size = 13),
      axis.text.x = element_text(size = 12),
      axis.title.y = element_text(size = 13),
      axis.text.y = element_text(size = 12),
      rect = element_rect(fill = "white")
      # plot.margin = unit(rep(0.5,4), 'lines'),
      # panel.background = element_rect(fill = 'transparent', color =
      #                                   'transparent'),
      # panel.border = element_rect(fill = 'transparent', color = 'transparent'),
      # panel.grid = element_blank(),
      # axis.title = element_text(color = 'black', vjust = 0.1),
      # axis.ticks.length = unit(-0.3, "lines"),
      # axis.ticks = element_line(colour = "grey20"),
      # legend.title = element_blank(),
      # legend.key = element_rect(fill = 'transparent', color = 'transparent')
    )
}


# theme_sxtdark <- function(...) {
#   require(grid)
#   ggplot2::theme_bw(...) +
#     ggplot2::theme(
#       axis.title.x = element_text(size = 13, colour = "white"),
#       axis.text.x = element_text(size = 12, colour = "white"),
#       axis.title.y = element_text(size = 13, colour = "white"),
#       axis.text.y = element_text(size = 12, colour = "white"),
#       rect = element_rect(fill = "black"),
#       # plot.margin = unit(rep(0.5,4), 'lines'),
#       panel.background = element_rect(fill = 'transparent', color =
#                                         'transparent'),
#       panel.border = element_rect(fill = 'white', color = 'white'),
#       panel.grid = element_blank(),
#       axis.title = element_text(color = 'white'),
#       # axis.ticks.length = unit(-0.3, "lines"),
#       axis.ticks = element_line(colour = "white"),
#       legend.title = element_text(color = "white"),
#       legend.key = element_rect(fill = 'white', color = 'white')
#     )
# }
