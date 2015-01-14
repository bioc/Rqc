\name{perCycleBasecall}
\alias{perCycleBasecall}
\title{
    Frequency distribution of cycle-specific base call
}
\description{
    This function combines frequency tables of all \code{RqcResultSet} objects
    in one table and also add file name information.
}
\usage{
    perCycleBasecall(rqcResultSet)
}
\arguments{
    \item{rqcResultSet}{
        list of \code{RqcResultSet} objects created by \code{\link{rqc}} 
        and \code{\link{rqcQA}} functions.
    }
}
\value{
    A \code{data.frame} with 'cycle', 'base', 'count' and 'filename' columns. 
    'count' variable represents how many a 'base' was identified on determined
    'cycle'.
}
\author{
    Welliton Souza
}
\examples{
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
files <- list.files(folder, "fastq.gz", full.names=TRUE)
rqcResultSet <- rqcQA(files)
df <- perCycleBasecall(rqcResultSet)
head(df)
qplot(x=base, y=count, fill=base, data=df, geom="bar", stat="identity", 
    facets=~filename)
}
\keyword{ qc }
