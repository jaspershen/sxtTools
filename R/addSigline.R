#' @title addSigline
#' @description Add significant lines in boxplot or ohter plots.
#' @param point.left.up Left up point coordinates.
#' @param point.right.up Right up point coordinates
#' @param point.left.bottom Left bottom point coordinates.
#' @param point.right.bottom Right bottom point coordinates
#' @param line.width Width of lines.
#' @param lwd lwd
#' @param sig.label Significant label.
#' @param n.y Position for signigficant label.
#' @param cex Size for significant label.
#' @author Xiaotao Shen
#' @export

setGeneric(name = "addSigline",
           function(point.left.up = c(1, 100),
                    point.right.up = c(2, 100),
                    point.left.bottom = c(1, 50),
                    point.right.bottom = c(2, 50),
                    line.width = 0.5,
                    lwd = 1,
                    sig.label = "***",
                    n.y = 1.01,
                    cex = 1.3){
             ##horizon line
             segments(x0 = point.left.up[1],
                      y0 = point.left.up[2],
                      x1 = point.right.up[1],
                      y1  = point.right.up[2],
                      lwd = lwd)

             #v line left
             segments(x0 = point.left.up[1],
                      y0 = point.left.up[2],
                      x1 = point.left.bottom[1],
                      y1 = point.left.bottom[2],
                      lwd = lwd)

             #v line right
             segments(x0 = point.right.up[1],
                      y0 = point.right.up[2],
                      x1 = point.right.bottom[1],
                      y1 = point.right.bottom[2],
                      lwd = lwd)



             segments(x0 = point.left.bottom[1] - line.width/2,
                      y0 = point.left.bottom[2],
                      x1 = point.left.bottom[1] + line.width/2,
                      y1 = point.left.bottom[2],
                      lwd = lwd)

             segments(x0 = point.right.bottom[1] - line.width/2,
                      y0 = point.right.bottom[2],
                      x1 = point.right.bottom[1] + line.width/2,
                      y1 = point.right.bottom[2],
                      lwd = lwd)


             text(x = (point.left.up[1]+point.right.up[1])/2,
                  y = point.left.up[2]*n.y, labels = sig.label,
                  cex = cex)




           })