\name{rqcCycleQualityBoxPlot}
\alias{rqcCycleQualityBoxPlot}
\title{
  Per cycle quality boxplot
}
\description{
  Plots per cycle quality boxplot.
}
\usage{
rqcCycleQualityBoxPlot(rqcResultSet)
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
\code{\link{rqcCycleQualityBoxCalc}}
}
\examples{
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
files <- list.files(folder, "fastq.gz", full.names=TRUE)
rqcResultSet <- rqcQA(files)
rqcCycleQualityBoxPlot(rqcResultSet)
}
\keyword{ graphics }
\keyword{ qc }
