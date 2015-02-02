\name{rqcReadQualityBoxCalc}
\alias{rqcReadQualityBoxCalc}
\title{Per read mean quality percentiles calculation for boxplot}
\description{
    Calculates per read mean quality percentiles to create boxplot.
}
\usage{
rqcReadQualityBoxCalc(rqcResultSet)
}
\arguments{
  \item{rqcResultSet}{
    list of \code{RqcResultSet} objects created by \code{\link{rqc}} 
    and \code{\link{rqcQA}} functions.
  }
}
\value{
  A data frame ready to plot, it contains the following columns:
  
  \itemize{
    \item filename: name of input files
    \item min: minimum value
    \item ymin: 10\% of data
    \item lower: 25\% of data
    \item middle: 50\% of data, median
    \item upper: 75\% of data
    \item ymax: 90\% of data
    \item max: maximum value
  }
}
\author{
  Welliton Souza
}
\seealso{
\code{\link{rqcReadQualityBoxPlot}}
}
\examples{
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
files <- list.files(folder, "fastq.gz", full.names=TRUE)
rqcResultSet <- rqcQA(files)
df <- rqcReadQualityBoxCalc(rqcResultSet)
p <- qplot(x=filename, ymin=ymin, lower=lower, middle=middle, upper=upper, 
    ymax=ymax, data=df, geom="boxplot", stat="identity") + 
    coord_flip()
p
# add minimum and maximum values
p + geom_point(aes(y=min)) + geom_point(aes(y=max))
}
\keyword{ qc }
