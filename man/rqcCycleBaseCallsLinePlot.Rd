\name{rqcCycleBaseCallsLinePlot}
\alias{rqcCycleBaseCallsLinePlot}
\title{Per cycle base calls line plot}
\usage{
rqcCycleBaseCallsLinePlot(rqcResultSet)
}
\arguments{
  \item{rqcResultSet}{
    list of \code{RqcResultSet} objects created by \code{\link{rqc}} 
    and \code{\link{rqcQA}} functions.
  }
}
\description{
Creates a line graph of per cycle base calls.
}
\value{
Plot object from \code{\link{ggplot}} function.
}
\author{
  Welliton Souza
}
\examples{
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
files <- list.files(folder, "fastq.gz", full.names=TRUE)
rqcResultSet <- rqcQA(files)
rqcCycleBaseCallsLinePlot(rqcResultSet)
}
\seealso{
\code{\link{rqcCycleBaseCallsCalc}}
}
\keyword{graphics}
\keyword{qc}

