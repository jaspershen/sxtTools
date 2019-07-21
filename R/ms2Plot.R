#' @title ms2Plot
#' @description Plot ms2 spectrum.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param spectrum1 Spectrum 1.
#' @param spectrum2 Spectrum 2.
#' @param col1 Color 1.
#' @param col2 Color 2.
#' @param col3 Color 3.
#' @param xlim xlim.
#' @param lwd lwd.
#' @param cex.lab cex.lab.
#' @param cex.axis cex.axis.
#' @param xlab xlab.
#' @param ylab ylab.
#' @param real.int.cutoff real.int.cutoff.
#' @param ppm.ms2match ppm.ms2match.
#' @param bty bty
#' @param xaxs xaxs
#' @param yaxs yaxs
#' @param ... Other parameters.
#' @return Return a MS2 spectrum.
#' @export


setGeneric(name = "ms2Plot", def = function(spectrum1,
                                            spectrum2,
                                            col1 = "lightseagreen",
                                            col2 = "tomato",
                                            col3 = "grey",
                                            xlim = NULL,
                                            lwd = 2,
                                            cex.lab = 1.8,
                                            cex.axis = 1.5,
                                            xlab = "m/z",
                                            ylab = "Relative intensity",
                                            real.int.cutoff = 0,
                                            ppm.ms2match = 30,
                                            bty = "o",
                                            xaxs = "r",
                                            yaxs = "r",
                                            ...){
  if(missing(spectrum2)){
    spectrum1 <- as.data.frame(spectrum1)
    spectrum1[,2] <- as.numeric(spectrum1[,2])/max(as.numeric(spectrum1[,2]))
    spectrum1 <- spectrum1[spectrum1[,2] > real.int.cutoff,]

    mz <- as.numeric(spectrum1[,1])
    int <- as.numeric(spectrum1[,2])

    par(xpd = FALSE)
    if(is.null(xlim)){
      plot(mz, int, type = "h", lwd = lwd, col = col1,
           cex.lab = cex.lab, cex.axis = cex.axis,
           xlab = xlab, ylab = ylab,...)
    }else{
      plot(mz, int, type = "h", lwd = lwd, col = col1,
           cex.lab = cex.lab, cex.axis = cex.axis, xlim = xlim,
           xlab = xlab, ylab = ylab,...)
    }


  }else{
    spectrum1 <- as.data.frame(spectrum1)
    spectrum1[,2] <- as.numeric(spectrum1[,2])/max(as.numeric(spectrum1[,2]))
    spectrum1 <- spectrum1[spectrum1[,2] > real.int.cutoff,]

    spectrum2 <- as.data.frame(spectrum2)
    spectrum2[,2] <- as.numeric(spectrum2[,2])/max(as.numeric(spectrum2[,2]))
    spectrum2 <- spectrum2[spectrum2[,2] > real.int.cutoff,]


    note <- annotateMS2(spec = spectrum1, matched.spec = spectrum2,
                        ppm.ms2match = ppm.ms2match)
    color1 <- rep(col3, length(note))
    color1[note=="matched"] <- col1
    spectrum1 <- data.frame(spectrum1, note, stringsAsFactors = FALSE)

    note <- annotateMS2(spec = spectrum2, matched.spec = spectrum1,
                        ppm.ms2match = ppm.ms2match)
    color2 <- rep(col3, length(note))
    color2[note=="matched"] <- col2
    spectrum2 <- data.frame(spectrum2, note, stringsAsFactors = FALSE)

    if(is.null(xlim)){
      xlim <- c(min(c(spectrum1[,1],spectrum2[,1])),max(c(spectrum1[,1],spectrum2[,1])))
    }

    plot(0, col = "white", xlab = xlab, ylab=ylab,
         ylim = c(-1,1),
         xlim = xlim,
         cex.lab =cex.lab, cex.axis = cex.axis, bty = bty, xaxs = xaxs,
         yaxs = yaxs, ...)

    abline(h = 0, lwd = 1.5)

    points(x = spectrum1[,1], spectrum1[,2], type = "h", col = color1, lwd = lwd)
    points(x = spectrum2[,1], -spectrum2[,2], type = "h", col = color2, lwd = lwd)


  }
})



setGeneric(name = "annotateMS2",
           def = function(spec, matched.spec,
                          ppm.ms2match = 30){
             note <- rep(NA, nrow(spec))
             for(i in 1:length(note)){
               note[i] <-
                 ifelse(any(abs(as.numeric(spec[i,1]) - as.numeric(matched.spec[,1]))*10^6/ifelse(as.numeric(spec[i,1])>=400, as.numeric(spec[i,1]), 400) < ppm.ms2match), "matched", "no")
             }
             note
           })





#' @title ms2Plot2
#' @description Plot ms2 spectrum.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param spectrum1 Spectrum 1.
#' @param spectrum2 Spectrum 2.
#' @param range.mz range.mz
#' @param ppm.tol ppm.tol
#' @param mz.ppm.thr mz.ppm.thr
#' @param xlab xlab.
#' @param ylab ylab.
#' @param col1 Color 1.
#' @param col2 Color 2.
#' @param title.size title.size
#' @param axis.text.size axis.text.size.
#' @param real.int.cutoff real.int.cutoff.
#' @param legend.title.size legend.title.size
#' @param legend.text.size legend.text.size
#' @return Return a MS2 spectrum.
#' @export
setGeneric(name = "ms2Plot2",
           def = function(spectrum1,
                          spectrum2,
                          range.mz,
                          ppm.tol = 30,
                          mz.ppm.thr = 400,
                          xlab = "Mass to charge ratio (m/z)",
                          ylab = "Relative intensity",
                          col1 = "red",
                          col2 = "black",
                          title.size = 15,
                          lab.size = 15,
                          axis.text.size =15,
                          legend.title.size = 15,
                          legend.text.size = 15
           ){
             spectrum1[,1] <- as.numeric(spectrum1[,1])
             spectrum1[,2] <- as.numeric(spectrum1[,2])

             spectrum2[,1] <- as.numeric(spectrum2[,1])
             spectrum2[,2] <- as.numeric(spectrum2[,2])

             spectrum1[,2] <- spectrum1[,2]/max(spectrum1[,2])
             spectrum2[,2] <- spectrum2[,2]/max(spectrum2[,2])

             spectrum1 <- as.data.frame(spectrum1)
             spectrum2 <- as.data.frame(spectrum2)

             if(missing(range.mz)){
               range.mz <- c(min(spectrum1[,1], spectrum2[,1]),
                             max(spectrum1[,1], spectrum2[,1]))

             }

             matched.spec <- tinyTools::ms2Match(spectrum1,
                                                 spectrum2,
                                                 ppm.tol = ppm.tol,
                                                 mz.ppm.thr = mz.ppm.thr)
             matched.idx <- which(matched.spec[, "Lib.intensity"] > 0 &
                                    matched.spec[, "Exp.intensity"] > 0)
             require(ggplot2)
             plot <- ggplot(matched.spec) +
               geom_segment(mapping = aes(x = Exp.mz, y = Exp.intensity - Exp.intensity,
                                          xend = Exp.mz, yend = Exp.intensity),
                            colour = col2) +
               geom_point(data = matched.spec[matched.idx,,drop = FALSE],
                          mapping = aes(x = Exp.mz, y = Exp.intensity), colour = col2) +
               xlim(range.mz[1], range.mz[2]) +
               ylim(-1, 1) +
               labs(x = xlab, y = ylab) +
               theme_bw() +
               theme(
                 # axis.line = element_line(arrow = arrow()),
                 plot.title = element_text(color = "black", size = title.size,
                                           face = "plain",
                                           hjust = 0.5),
                 axis.title = element_text(color = "black", size = lab.size,
                                           face = "plain"),
                 axis.text = element_text(color = "black", size = axis.text.size,
                                          face = "plain"),
                 legend.title = element_text(color = "black", size = legend.title.size,
                                             face = "plain"),
                 legend.text = element_text(color = "black", size = legend.text.size,
                                            face = "plain")
               )

             plot <- plot +
               annotate(geom = "text", x = Inf, y = Inf,
                        label = "Spectrum 1",
                        color = col2,
                        hjust = 1, vjust = 1) +
               annotate(geom = "text", x = Inf, y = -Inf,
                        label = "Spectrum 2",
                        color = col1,
                        hjust = 1, vjust = -1)

             plot <- plot +
               geom_segment(data = matched.spec,
                            mapping = aes(x = Lib.mz, y = Lib.intensity - Lib.intensity,
                                          xend = Lib.mz, yend = -Lib.intensity),
                            colour = col1) +
               geom_point(data = matched.spec[matched.idx, , drop = FALSE],
                          mapping = aes(x = Lib.mz, y = -Lib.intensity), colour = col1)
             plot
           })







