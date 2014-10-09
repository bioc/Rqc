\name{rqcCycleQualityCalc}
\alias{rqcCycleQualityCalc}
\title{Per cycle quality calculation}
\description{
  Calculates per cycle quality percentiles.
}
\usage{
rqcCycleQualityCalc(rqcResultSet)
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
\code{\link{rqcCycleQualityPlot}}
}
\examples{
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
files <- list.files(folder, "fastq.gz", full.names=TRUE)
rqcResultSet <- rqcQA(files)
dfList <- rqcCycleQualityCalc(rqcResultSet)
df <- do.call(rbind, dfList)
qplot(x=cycle, y=percentiles, fill=value, data=df, geom="bar", 
      stat="identity", facets=~filename)
}
\keyword{qc}
