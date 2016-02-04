#' Main Rqc function
#' 
#' Rqc is an optimized tool designed for quality assessment of high-throughput
#' sequencing data. It performs parallel processing of entire files and
#' produces an HTML report, which contains a set of high-resolution images that
#' can be directly used on publications.
#' 
#' 
#' @param path directory path that contains input files.
#' @param pattern a regex expression that macthes to input file names
#' @param sample it reads a random sample from files if this parameter is TRUE.
#' @param n number of sequences to read from each input file.  This represents
#' sample size if 'sample' parameter is TRUE, if not represents the chunk size
#' to read on each iteration.  By default, it reads a sample of one million
#' sequences from each input file.
#' @param groupFactor group name for each input file.
#' @param outdir output directory path.  Is created a temporary directory by
#' default.
#' @param file output file name.
#' @param openBrowser if TRUE opens report file on default Internet Browser.
#' @param workers Number of parallel workers. Set 1 to serial. Default value
#' from \code{\link{multicoreWorkers}}.
#' @return A invisible named list of \code{RqcResultSet} objects, each one
#' represents a file.
#' @author Welliton Souza
#' @seealso \code{\link{rqcQA}}
#' @keywords graphics qc
#' @examples
#' 
#'   options(device.ask.default = FALSE)
#'   folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
#'   rqc(folder, ".fastq.gz", workers=1, openBrowser=FALSE)
#' 
#' @export
rqc <- function(path=".", pattern,
                sample=TRUE, n=1e6,
                groupFactor = rep("None", length(files)),
                outdir=tempdir(), file="rqc_report",
                openBrowser=TRUE,
                workers=multicoreWorkers())
{
    files <- list.files(path, pattern, full.names=TRUE)
    rqcResultSet <- rqcQA(files, sample, n, groupFactor, workers)
    reportFile <- rqcReport(rqcResultSet, outdir, file)
    message(sprintf("'%s' has been created.", reportFile))
    if (openBrowser) {
        browseURL(reportFile)
    }
    invisible(rqcResultSet)
}
