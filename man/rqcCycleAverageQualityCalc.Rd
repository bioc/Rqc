\name{rqcCycleAverageQualityCalc}
\alias{rqcCycleAverageQualityCalc}
\title{Per cycle average quality calculation}
\description{
Calculates average quality per cycle.
}
\usage{
rqcCycleAverageQualityCalc(rqcResultSet)
}
\arguments{
  \item{rqcResultSet}{
    list of \code{RqcResultSet} objects created by \code{\link{rqc}} 
    and \code{\link{rqcQA}} functions.
  }
}
\value{
Data frame ready for plot.
}
\author{
Welliton Souza
}
\examples{
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
files <- list.files(folder, "fastq.gz", full.names=TRUE)
rqcResultSet <- rqcQA(files)
df <- rqcCycleAverageQualityCalc(rqcResultSet)
df$cycle <- as.numeric(levels(df$cycle))[df$cycle]
qplot(x=cycle, y=quality, colour=filename, data=df, geom="line")
}
\seealso{
\code{\link{rqcCycleAverageQualityPlot}}
}
\keyword{qc}
