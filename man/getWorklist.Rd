% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getWorklist.R
\name{getWorklist}
\alias{getWorklist}
\title{getWorklist}
\usage{
getWorklist(
  table.name = "batch1.xlsx",
  instrument = c("Thermo", "Agilent", "AB"),
  each.mode.number = 30,
  randommethod = c("no", "position", "injection"),
  samplenumber = NULL,
  replication = 1,
  QCstep = 8,
  conditionQCnumber = 5,
  qc.index.from = 1,
  dir = "D:\\\\Liang\\\\SmarD_Urine_20190731\\\\RPLC\\\\",
  method.path = "D:\\\\Liang\\\\Methods\\\\RPLC\\\\Urine\\\\",
  ms1.method.pos = "Hypersil_GOLD_MS_pos",
  ms1.method.neg = "Hypersil_GOLD_MS_neg",
  ms2.method.pos = c("Hypersil_GOLD_MSMS_pos_NCE25_70_250",
    "Hypersil_GOLD_MSMS_pos_NCE25_240_400", "Hypersil_GOLD_MSMS_pos_NCE25_390_600",
    "Hypersil_GOLD_MSMS_pos_NCE25_590_1000", "Hypersil_GOLD_MSMS_pos_NCE50_70_250",
    "Hypersil_GOLD_MSMS_pos_NCE50_240_400", "Hypersil_GOLD_MSMS_pos_NCE50_390_600",
    "Hypersil_GOLD_MSMS_pos_NCE50_590_1000"),
  ms2.method.neg = c("Hypersil_GOLD_MSMS_neg_NCE25_70_250",
    "Hypersil_GOLD_MSMS_neg_NCE25_240_400", "Hypersil_GOLD_MSMS_neg_NCE25_390_600",
    "Hypersil_GOLD_MSMS_neg_NCE25_590_1000", "Hypersil_GOLD_MSMS_neg_NCE50_70_250",
    "Hypersil_GOLD_MSMS_neg_NCE50_240_400", "Hypersil_GOLD_MSMS_neg_NCE50_390_600",
    "Hypersil_GOLD_MSMS_neg_NCE50_590_1000")
)
}
\arguments{
\item{instrument}{Which instrument you use?
"Agilent", "AB" or "Thermo", default is "Thermo".}

\item{randommethod}{Which random method you want to use?
"no", "position" or "injection". Default is "no".}

\item{samplenumber}{Sample number.}

\item{replication}{Replication times.}

\item{QCstep}{QC step.}

\item{conditionQCnumber}{Condition QC number.}

\item{dir}{Directory.}

\item{name}{The name of worklist.}

\item{validationQCstep}{Validation QC step.}

\item{testmixstep}{Test mixture step.}

\item{injectionfrom}{Injection order from which? Default is 1.}

\item{user}{Default is "other".}
}
\value{
New worklist.
}
\description{
Generate Worklist for MS data acquisition.
}
\author{
Xiaotao Shen
\email{shenxt@sioc.ac.cn}
}
