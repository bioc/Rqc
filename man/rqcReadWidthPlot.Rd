\name{rqcReadWidthPlot}
\alias{rqcReadWidthPlot}
\title{
  Per read width plot
}
\description{
  Creates bar graph of per read width from all elements of input list.
}
\usage{
rqcReadWidthPlot(rqcResultSet)
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
\code{\link{rqcReadWidthCalc}}
}
\examples{
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
files <- list.files(folder, "fastq.gz", full.names=TRUE)
rqcResultSet <- rqcQA(files)
rqcReadWidthPlot(rqcResultSet)
}
\keyword{ graphics }
\keyword{ qc }
