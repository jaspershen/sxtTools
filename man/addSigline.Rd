% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addSigline.R
\name{addSigline}
\alias{addSigline}
\title{addSigline}
\usage{
addSigline(
  point.left.up = c(1, 100),
  point.right.up = c(2, 100),
  point.left.bottom = c(1, 50),
  point.right.bottom = c(2, 50),
  line.width = 0.5,
  lwd = 1,
  sig.label = "***",
  n.y = 1.01,
  cex = 1.3
)
}
\arguments{
\item{point.left.up}{Left up point coordinates.}

\item{point.right.up}{Right up point coordinates}

\item{point.left.bottom}{Left bottom point coordinates.}

\item{point.right.bottom}{Right bottom point coordinates}

\item{line.width}{Width of lines.}

\item{lwd}{lwd}

\item{sig.label}{Significant label.}

\item{n.y}{Position for signigficant label.}

\item{cex}{Size for significant label.}
}
\description{
Add significant lines in boxplot or ohter plots.
}
\author{
Xiaotao Shen
}
