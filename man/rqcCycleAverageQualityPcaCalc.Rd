\name{rqcCycleAverageQualityPcaCalc}
\alias{rqcCycleAverageQualityPcaCalc}
\title{
    PCA calculation of per cycle read average quality
}
\description{
    This function calculates Principal Components of per cycle read average quality.
}
\usage{
    rqcCycleAverageQualityPcaCalc(rqcResultSet)
}
\arguments{
  \item{rqcResultSet}{
    list of \code{RqcResultSet} objects created by \code{\link{rqc}} 
    and \code{\link{rqcQA}} functions.
}
}
\value{
    An object of class prcomp.
}
\author{
    Welliton Souza
}
\examples{
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
files <- list.files(folder, "fastq.gz", full.names=TRUE)
rqcResultSet <- rqcQA(files)
fit <- rqcCycleAverageQualityPcaCalc(rqcResultSet)
biplot(fit)
}
\keyword{ qc }
