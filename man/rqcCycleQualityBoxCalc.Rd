\name{rqcCycleQualityBoxCalc}
\alias{rqcCycleQualityBoxCalc}
\title{Per cycle quality percentiles calculation for boxplot}
\description{
    Calculates per cycle quality percentiles to create boxplot.
}
\usage{
rqcCycleQualityBoxCalc(rqcResultSet)
}
\arguments{
  \item{rqcResultSet}{
    list of \code{RqcResultSet} objects created by \code{\link{rqc}} 
    and \code{\link{rqcQA}} functions.
  }
}
\value{
    Data frame ready to plot.
}
\author{
    Welliton Souza
}
\seealso{
\code{\link{rqcCycleQualityBoxPlot}}
}
\examples{
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
files <- list.files(folder, "fastq.gz", full.names=TRUE)
rqcResultSet <- rqcQA(files)
df <- rqcCycleQualityBoxCalc(rqcResultSet)
qplot(x=cycle, ymin=ymin, lower=lower, middle=middle, upper=upper, ymax=ymax, 
      data=df, geom="boxplot", stat="identity", facets=~filename)
}
\keyword{ qc }