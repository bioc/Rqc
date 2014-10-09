\name{rqcCycleGCPlot}
\alias{rqcCycleGCPlot}
\title{Per cycle percentual GC plot}
\usage{
rqcCycleGCPlot(rqcResultSet)
}
\arguments{
  \item{rqcResultSet}{
    list of \code{RqcResultSet} objects created by \code{\link{rqc}} 
    and \code{\link{rqcQA}} functions.
  }
}
\description{
Creates a line graph of per cycle percentual GC. 
}
\value{Plot object from \code{\link{ggplot}} function.}
\author{
  Welliton Souza
}
\seealso{
\code{\link{rqcCycleGCCalc}}
}
\examples{
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
files <- list.files(folder, "fastq.gz", full.names=TRUE)
rqcResultSet <- rqcQA(files)
rqcCycleGCPlot(rqcResultSet)
}
\keyword{graphics}
\keyword{qc}

