\name{rqcQA}
\alias{rqcQA}
\title{Quality Assessment Rqc function}
\usage{
  rqcQA(files, sample = TRUE, n = 1e6, BPPARAM = bpparam())
}
\arguments{
  \item{files}{A vector of file paths.}
  \item{sample}{It reads a random sample from files if this parameter is TRUE.}
  \item{n}{Number of sequences to read from each input file.
  This represents sample size if 'sample' parameter is TRUE,
  if not represents the chunk size to read on each iteration.
  Default is read a sample of one million sequences from each input file.}
  \item{BPPARAM}{An instance of a \code{BiocParallelParam}. See \code{\link{bpparam}}.}
}
\value{
  A named list of \code{RqcResultSet} objects, each one represents a file.
}
\description{
  Process a set of files and returns a list of quality control data.
  Files must be FASTQ format, compressed or not.
}
\details{
  Input files are read using \code{FastStreamer} and 
  \code{FastSampler} classes of \code{\link{ShortRead}} package.
  Process multiple files in parallel using \code{\link{bplapply}} function 
  of \code{\link{BiocParallel}} package.
}
\seealso{
\code{\link{rqc}}
}
\examples{
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
files <- list.files(full.names=TRUE, path=folder)
rqcResultSet <- rqcQA(files)
rqcReadQualityPlot(rqcResultSet)
}
\author{
  Welliton Souza
}

