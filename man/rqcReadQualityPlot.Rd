\name{rqcReadQualityPlot}
\alias{rqcReadQualityPlot}
\title{
  Per read quality plot
}
\description{
  Plots the quality of all the files by read.
}
\usage{
rqcReadQualityPlot(rqcResultSet)
}
\arguments{
  \item{rqcResultSet}{
    list of \code{RqcResultSet} objects created by \code{\link{rqc}} 
    and \code{\link{rqcQA}} functions.
  }
}
\value{
  Plot object from \code{\link{ggplot}} function.
}
\author{
  Welliton Souza
}
\seealso{
\code{\link{rqcReadQualityCalc}}
}
\examples{
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
files <- list.files(folder, "fastq.gz", full.names=TRUE)
rqcResultSet <- rqcQA(files)
rqcReadQualityPlot(rqcResultSet)
}
\keyword{graphics}
\keyword{qc}
