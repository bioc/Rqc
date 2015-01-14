\name{perTileQuality}
\alias{perTileQuality}
\title{
    Mean quality by tile and lane
}
\description{
    This function conbines tables of all \code{RqcResultSet} objects.
}
\usage{
    perTileQuality(rqcResultSet)
}
\arguments{
    \item{rqcResultSet}{
        list of \code{RqcResultSet} objects created by \code{\link{rqc}} 
        and \code{\link{rqcQA}} functions.
    }
}
\value{
    A \code{data.frame} with 'lane', 'tile', average' and 'filename' columns.
}
\author{
    Welliton Souza
}
\examples{
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
files <- list.files(folder, "fastq.gz", full.names=TRUE)
rqcResultSet <- rqcQA(files)
df <- perTileQuality(rqcResultSet)
head(df)
}
\keyword{ qc }