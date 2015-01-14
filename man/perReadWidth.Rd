\name{perReadWidth}
\alias{perReadWidth}
\title{
    Frequency distribution of read width
}
\description{
    This function combines frequency table of all \code{RqcResultSet} objects in
    one table and also add file name information.
}
\usage{
    perReadWidth(rqcResultSet)
}
\arguments{
    \item{rqcResultSet}{
        list of \code{RqcResultSet} objects created by \code{\link{rqc}} 
        and \code{\link{rqcQA}} functions.
    }
}
\value{
    A \code{data.frame} with 'width', 'count' and 'filename' columns. 'count'
    variable represents haw many reads have the same 'width'.
}
\author{
    Welliton Souza
}
\examples{
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
files <- list.files(folder, "fastq.gz", full.names=TRUE)
rqcResultSet <- rqcQA(files)
df <- perReadWidth(rqcResultSet)
head(df)
}
\keyword{ qc }
