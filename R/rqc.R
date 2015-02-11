rqc <- function(path=".", pattern,
                sample=TRUE, n=1e6,
                groupFactor = rep("None", length(files)),
                outdir=tempdir(), file="rqc_report",
                openBrowser=TRUE,
                BPPARAM=bpparam())
{
    files <- list.files(path, pattern, full.names=TRUE)
    rqcResultSet <- rqcQA(files, sample, n, groupFactor, BPPARAM=BPPARAM)
    reportFile <- rqcReport(rqcResultSet, outdir, file)
    message(sprintf("'%s' has been created.", reportFile))
    if (openBrowser) {
        browseURL(reportFile)
    }
    invisible(rqcResultSet)
}