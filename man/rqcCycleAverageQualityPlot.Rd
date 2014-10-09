\name{rqcCycleAverageQualityPlot}
\alias{rqcCycleAverageQualityPlot}
\title{Per cycle average quality plot}
\description{
Creates a line graph of per cycle average quality.
}
\usage{
rqcCycleAverageQualityPlot(rqcResultSet)
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
\examples{
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
files <- list.files(folder, "fastq.gz", full.names=TRUE)
rqcResultSet <- rqcQA(files)
rqcCycleAverageQualityPlot(rqcResultSet)
}
\seealso{
\code{\link{rqcCycleAverageQualityCalc}}
}
\keyword{graphics}
\keyword{qc}
