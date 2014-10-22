rqcReport <- function(rqcResultSet, outdir=tempdir(), file="rqc_report")
{
    outdir <- normalizePath(outdir)
    output <- file.path(outdir, paste0(file, '.html'))
    figDir <- paste0(tempdir(), "/rqc-")
    rmdFile <- system.file("templates", package="Rqc", "rqc_report.Rmd")
    knit2html(rmdFile, output, quiet = TRUE, options=c("base64_images"))
    output
}
