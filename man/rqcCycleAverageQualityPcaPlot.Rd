\name{rqcCycleAverageQualityPcaPlot}
\alias{rqcCycleAverageQualityPcaPlot}
\title{
    Biplot of PCA of per cycle read average quality
}
\description{
    This function creates a Biplot of PCA of per cycle read average quality
}
\usage{
rqcCycleAverageQualityPcaPlot(rqcResultSet)
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
\code{\link{rqcCycleAverageQualityPcaCalc}}
}
\examples{
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
files <- list.files(folder, "fastq.gz", full.names=TRUE)
rqcResultSet <- rqcQA(files)
rqcCycleAverageQualityPcaPlot(rqcResultSet)
}
\keyword{ qc }
\keyword{ graphics }% __ONLY ONE__ keyword per line
