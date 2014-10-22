\name{rqcReport}
\alias{rqcReport}
\title{Quality Control HTML Report}
\usage{
rqcReport(rqcResultSet, outdir=tempdir(), file="rqc_report")
}
\arguments{
  \item{rqcResultSet}{
    list of \code{RqcResultSet} objects created by \code{\link{rqc}} 
    and \code{\link{rqcQA}} functions.
  }
  \item{outdir}{output directory path. 
  Is created a temporary directory by default.}
  \item{file}{output file name.}
}
\value{
  Report file path.
}
\description{
  Generates an HTML report file. 
  
  Also creates a directory called "figure" in \code{outdir} path.
}
\author{
  Welliton Souza
}
\examples{
  options(device.ask.default = FALSE)
  folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
  files <- list.files(full.names=TRUE, path=folder)
  rqcResultSet <- rqcQA(files)
  reportFile <- rqcReport(rqcResultSet)
  browseURL(reportFile)
}
\seealso{
\code{\link{rqc}}

\code{\link{rqcQA}}
}

