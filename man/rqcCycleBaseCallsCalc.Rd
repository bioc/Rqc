\name{rqcCycleBaseCallsCalc}
\alias{rqcCycleBaseCallsCalc}
\title{Per cycle base call calculation}
\usage{
rqcCycleBaseCallsCalc(rqcResultSet)
}
\description{
Calculates average base call per cycle.
}
\arguments{
  \item{rqcResultSet}{
    list of \code{RqcResultSet} objects created by \code{\link{rqc}} 
    and \code{\link{rqcQA}} functions.
  }
}
\value{ List of data frames ready for plot. }
\examples{
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
files <- list.files(folder, "fastq.gz", full.names=TRUE)
rqcResultSet <- rqcQA(files)
df <- rqcCycleBaseCallsCalc(rqcResultSet)
qplot(x=cycle, y=value, colour=variable, group=variable, data=df, 
      geom="line", facets=~filename)
}
\seealso{
\code{\link{rqcCycleBaseCallsPlot}}
}
\author{Welliton Souza}
\keyword{qc}
