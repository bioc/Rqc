\docType{package}
\name{Rqc-package}
\alias{Rqc}
\alias{Rqc-package}
\title{Quality Control Tool for High-Throughput Sequencing Data}
\description{
Rqc is an optimized tool designed for quality control and assessment of high-throughput sequencing data. It performs parallel processing of entire files and produces a report which contains a set of high-resolution graphics.
}
\details{
  \tabular{ll}{
  Package: \tab Rqc \cr 
  Type:    \tab Package \cr
  Version: \tab 0.9 \cr 
  Date:    \tab 2014-08-23 \cr 
  License: \tab GPL (>= 2) \cr }
}
\examples{
options(device.ask.default = FALSE)
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
rqc(folder, ".fastq.gz")
}
\author{
  Welliton Souza, Benilton Carvalho

  Maintainer: Welliton Souza <well309@gmail.com>
}
\keyword{package}

