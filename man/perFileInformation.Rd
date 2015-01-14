\name{perFileInformation}
\alias{perFileInformation}
\title{
    File information
}
\description{
    This function combines file information (such as name and path) of all
    \code{RqcResultSet} objects in one \code{data.frame}.
}
\usage{
    perFileInformation(rqcResultSet)
}
\arguments{
    \item{rqcResultSet}{
        list of \code{RqcResultSet} objects created by \code{\link{rqc}} 
        and \code{\link{rqcQA}} functions.
    }
}
\value{
    A \code{data.frame} containing information abount files.
}
\author{
    Welliton Souza
}
\examples{
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
files <- list.files(folder, "fastq.gz", full.names=TRUE)
rqcResultSet <- rqcQA(files)
perFileInformation(rqcResultSet)
}
\keyword{ qc }
