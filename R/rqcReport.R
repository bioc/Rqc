rqcReport <- function(rqcResultSet, outdir=tempdir(), file="rqc_report", keepMD=FALSE)
{
    outdir <- normalizePath(outdir)
    figDir <- paste0(tempdir(), "/rqc-")
    mdFile <- file.path(outdir, paste0(file, '.md'))
    htmlFile <- file.path(outdir, paste0(file, '.html'))
    rmdFile <- system.file("templates", package="Rqc", "rqc_report.Rmd")
    knit(rmdFile, mdFile, quiet=TRUE)
    markdownToHTML(mdFile, htmlFile, options="base64_images")
    if (!keepMD) unlink(mdFile)
    htmlFile
}
