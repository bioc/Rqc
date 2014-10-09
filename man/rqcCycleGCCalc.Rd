\name{rqcCycleGCCalc}
\alias{rqcCycleGCCalc}
\title{
  Per cycle percentual GC.
}
\description{
  Calculates per cycle percentual GC.
}
\usage{
rqcCycleGCCalc(rqcResultSet)
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
\code{\link{rqcCycleGCPlot}}
}
\examples{
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
files <- list.files(folder, "fastq.gz", full.names=TRUE)
rqcResultSet <- rqcQA(files)
df <- rqcCycleGCCalc(rqcResultSet)
qplot(x=cycle, y=gc, colour=filename, data=df, geom="line")
}
\keyword{qc}