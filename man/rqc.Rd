\name{rqc}
\alias{rqc}
\title{ Main Rqc function }
\description{
Rqc is an optimized tool designed for quality control and assessment of high-throughput sequencing data. It performs parallel processing of entire files and produces a report which contains a set of high-resolution graphics.

Performs quality control assay on a list of FASTQ files in paralell 
and creates a HTML report.
}
\usage{
rqc(path=".", pattern,
    sample=TRUE, n=1e6,
    outdir=tempdir(), file="rqc_report",
    openBrowser=TRUE)
}
\arguments{
  \item{path}{directory path that contains input files.}
  \item{pattern}{a regex expression that macthes to input file names}
  \item{sample}{it reads a random sample from files if this parameter is TRUE.}
  \item{n}{number of sequences to read from each input file.
  This represents sample size if 'sample' parameter is TRUE,
  if not represents the chunk size to read on each iteration.
  By default, it reads a sample of one million sequences from each input file.}
  \item{outdir}{output directory path. 
  Is created a temporary directory by default.}
  \item{file}{output file name.}
  \item{openBrowser}{if TRUE opens report file on default Internet Browser.}
}
\value{
  A invisible named list of \code{RqcResultSet} objects, 
  each one represents a file.
}
\author{
  Welliton Souza
}
\seealso{
\code{\link{rqcQA}}
}
\examples{
options(device.ask.default = FALSE)
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
rqc(folder, ".fastq.gz")
}
\keyword{graphics}
\keyword{qc}