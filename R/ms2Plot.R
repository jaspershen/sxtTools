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








