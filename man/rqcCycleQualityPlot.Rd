\name{rqcCycleQualityPlot}
\alias{rqcCycleQualityPlot}
\title{Per cycle quality plot}
\usage{
rqcCycleQualityPlot(rqcResultSet)
}
\arguments{
  \item{rqcResultSet}{
    list of \code{RqcResultSet} objects created by \code{\link{rqc}} 
    and \code{\link{rqcQA}} functions.
  }
}
\description{
  Creates a graph of per cycle quality.
}
\value{
  Plot object from \code{\link{ggplot}} function.
}
\author{
  Welliton Souza
}
\seealso{
\code{\link{rqcCycleQualityCalc}}
}
\examples{
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
files <- list.files(folder, "fastq.gz", full.names=TRUE)
rqcResultSet <- rqcQA(files)
rqcCycleQualityPlot(rqcResultSet)
}
\keyword{graphics}
\keyword{qc}

