\name{perReadQuality}
\alias{perReadQuality}
\title{
    Frequency distribution of per read mean quality
}
\description{
    This function returns a frequence distribution of per read mean quality from
    all \code{RqcResultSet} objects included on input list.
}
\usage{
    perReadQuality(rqcResultSet)
}
\arguments{
    \item{rqcResultSet}{
        list of \code{RqcResultSet} objects created by \code{\link{rqc}} 
        and \code{\link{rqcQA}} functions.
    }
}
\value{
    A \code{data.frame} with 'average', 'count' and 'filename' columns. 'count'
    variable represents how many the 'average' values are found.
}
\author{
    Welliton Souza
}
\examples{
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
files <- list.files(folder, "fastq.gz", full.names=TRUE)
rqcResultSet <- rqcQA(files)
df <- perReadQuality(rqcResultSet)
head(df)
# create a histogram
qplot(x=average, weight=count, data=df, geom="histogram", binwidth=1, 
      facets=~filename)
}

\keyword{ qc }