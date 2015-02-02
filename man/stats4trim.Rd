\name{stats4trim}
\alias{stats4trim}
\title{
    Minimun read mean quality and maximum percentage loss of reads estimations for trimming step.
}
\description{
    This function estimates how many reads would be lost if the sequences are filtered by a minimum read mean quality value. Also this function estimates what is the minimum read mean quality value for filtering and lose max percentage defined.
}
\usage{
    stats4trim(rqcResultSet, qmin, pmax)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{rqcResultSet}{
    list of \code{RqcResultSet} objects created by \code{\link{rqc}} 
    and \code{\link{rqcQA}} functions.
}
  \item{qmin}{
    Minimum read mean quality value (bewteen 0 and 41).
}
  \item{pmax}{
    Maximum percentage of reads permitted been lost during trimming step.
}
}
\value{
    A data frame containg estimated minimum quality and maximum percentage for each input file.
}
\author{
    Welliton Souza
}
\examples{
folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
files <- list.files(folder, "fastq.gz", full.names=TRUE)
rqcResultSet <- rqcQA(files)
stats4trim(rqcResultSet, qmin=20)
stats4trim(rqcResultSet, pmax=10)
}
\keyword{ qc }
\keyword{ trimming }
