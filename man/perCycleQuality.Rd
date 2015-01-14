\name{perCycleQuality}
\alias{perCycleQuality}
\title{
    Frequency distribution of cycle-specific quality
}
\description{
    This function combines frequency tables of all \code{RqcResultSet} objects 
    in one table and also add file name information.
}
\usage{
    perCycleQuality(rqcResultSet)
}
\arguments{
    \item{rqcResultSet}{
        list of \code{RqcResultSet} objects created by \code{\link{rqc}} 
        and \code{\link{rqcQA}} functions.
    }
}
\value{
    A \code{data.frame} with 'cycle', 'quality', 'score', 'count' and 'filename'
    columns. 'count' variable represents how many a 'quality/score' was 
    identified on determined 'cycle'.
}
\author{
    Welliton Souza
}
\examples{
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
files <- list.files(folder, "fastq.gz", full.names=TRUE)
rqcResultSet <- rqcQA(files)
df <- perCycleQuality(rqcResultSet)
head(df)
qplot(x=quality, y=count, data=df, geom="bar", stat="identity", 
    facets=~filename)
}
\keyword{ qc }
