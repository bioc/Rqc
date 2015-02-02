\name{rqcReadQualityBoxPlot}
\alias{rqcReadQualityBoxPlot}
\title{
    Per read mean quality box plot
}
\description{
    This function creates crate a graphic charts with box plots describing per 
    read mean quality distribution for each input file 
}
\usage{
rqcReadQualityBoxPlot(rqcResultSet)
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
\code{\link{rqcReadQualityBoxCalc}}
}
\examples{
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
files <- list.files(folder, "fastq.gz", full.names=TRUE)
rqcResultSet <- rqcQA(files)
rqcReadQualityBoxPlot(rqcResultSet)
}
\keyword{ graphics }
\keyword{ qc }
