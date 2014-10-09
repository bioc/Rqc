rqc <- function(path=".", pattern,
                sample=TRUE, n=1e6,
                outdir=tempdir(), file="rqc_report",
                openBrowser=TRUE)
{
    files <- list.files(path, pattern, full.names=TRUE)
    rqcResultSet <- rqcQA(files, sample, n)
    reportFile <- rqcReport(rqcResultSet, outdir, file)
    message(sprintf("'%s' has been created.", reportFile))
    if (openBrowser) {
        browseURL(reportFile)
    }
    invisible(rqcResultSet)
}