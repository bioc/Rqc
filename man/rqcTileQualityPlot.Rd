\name{rqcTileQualityPlot}
\alias{rqcTileQualityPlot}
\title{
  Per tile and lane quality Plot
}
\description{
  Creates a plot.
}
\usage{
  rqcTileQualityPlot(rqcResultSet)
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
rqcTileQualityPlot(rqcResultSet)
}
\keyword{ qc }
\keyword{ graphics }% __ONLY ONE__ keyword per line
