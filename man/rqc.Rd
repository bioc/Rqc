\name{rqc}
\alias{rqc}
\title{Main Rqc function }
\description{
Rqc is an optimized tool designed for quality assessment
of high-throughput sequencing data. It performs parallel processing of
entire files and produces an HTML report, which contains a set of
high-resolution images that can be directly used on publications.
}
\usage{
rqc(path=".", pattern,
    sample=TRUE, n=1e6,
    outdir=tempdir(), file="rqc_report",
    openBrowser=TRUE,
    BPPARAM=bpparam())
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
  \item{BPPARAM}{An instance of a \code{BiocParallelParam}. See \code{\link{bpparam}}.}
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
  rqc(folder, ".fastq.gz", openBrowser=FALSE)
}
\keyword{graphics}
\keyword{qc}
