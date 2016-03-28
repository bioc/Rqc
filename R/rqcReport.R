#' Quality Control HTML Report
#' 
#' Generates an HTML report file.
#' 
#' Also creates a directory called "figure" in \code{outdir} path.
#' 
#' 
#' @param rqcResultSet list of \code{RqcResultSet} objects created by
#' \code{\link{rqc}} and \code{\link{rqcQA}} functions.
#' @param outdir output directory path.  It is created a temporary directory by
#' default.
#' @param file output file name.
#' @param keepMD If true Rqc does not delete markdown file.  \code{\link{knit}}
#' function takes RMarkdown template file (within package) and generates a
#' temporary Markdown file. Next \code{\link{markdownToHTML}} function takes
#' this markdown file and creates final HTML file.
#' @param templateFile Path of Rmarkdown file as Rqc web report template.
#' @return Report file path.
#' @author Welliton Souza
#' @seealso \code{\link{rqc}}
#' 
#' \code{\link{rqcQA}}
#' @examples
#' 
#' options(device.ask.default = FALSE)
#' checkpoint("Rqc", path=system.file(package="Rqc", "extdata"), {
#'   folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
#'   files <- list.files(full.names=TRUE, path=folder)
<<<<<<< HEAD
#'   rqcResultSet <- rqcQA(files, workers=1)
=======
#'   rqcResultSet <- rqcQA(files, pair=c(1,1), workers=1)
>>>>>>> master
#' }, keep="rqcResultSet")
#' reportFile <- rqcReport(rqcResultSet)
#' browseURL(reportFile)
#' 
#' @export
rqcReport <- function(rqcResultSet, outdir=tempdir(), file="rqc_report", keepMD=FALSE,
                      templateFile=system.file("templates", package="Rqc", "rqc_report.Rmd"))
{
    outdir <- normalizePath(outdir)
    figDir <- paste0(tempdir(), "/rqc-")
    mdFile <- file.path(outdir, paste0(file, '.md'))
    htmlFile <- file.path(outdir, paste0(file, '.html'))
    knit(templateFile, mdFile, quiet=TRUE)
    markdownToHTML(mdFile, htmlFile, options="base64_images")
    if (!keepMD) unlink(mdFile)
    htmlFile
}
