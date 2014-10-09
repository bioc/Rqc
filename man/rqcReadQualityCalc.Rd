\name{rqcReadQualityCalc}
\alias{rqcReadQualityCalc}
\title{
  Per read quality calculation
}
\description{
  Calculates per read quality
}
\usage{
rqcReadQualityCalc(rqcResultSet)
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
\code{\link{rqcReadQualityPlot}}
}
\examples{
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
files <- list.files(folder, "fastq.gz", full.names=TRUE)
rqcResultSet <- rqcQA(files)
df <- rqcReadQualityCalc(rqcResultSet)
qplot(x=quantile, y=value, colour=filename, data=df, geom="line")
}
\keyword{ qc }
