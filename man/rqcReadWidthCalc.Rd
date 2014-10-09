\name{rqcReadWidthCalc}
\alias{rqcReadWidthCalc}
\title{
  Per read width calculation
}
\description{
  Calculates amount of per read width
}
\usage{
rqcReadWidthCalc(rqcResultSet)
}
\arguments{
  \item{rqcResultSet}{
    list of \code{RqcResultSet} objects created by \code{\link{rqc}} 
    and \code{\link{rqcQA}} functions.
  }
}
\value{ Data frame ready for plot. }
\author{
  Welliton Souza
}
\seealso{
\code{\link{rqcReadWidthPlot}}
}
\examples{
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
files <- list.files(folder, "fastq.gz", full.names=TRUE)
rqcResultSet <- rqcQA(files)
df <- rqcReadWidthCalc(rqcResultSet)
qplot(x=width, y=count, data=df, geom="bar", stat="identity", facets=~filename)
}
\keyword{ qc }
