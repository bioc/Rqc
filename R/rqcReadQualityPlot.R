#' Per read quality plot
#' 
#' Plots the quality of all the files by read.
#' 
#' @param rqcResultSet list of \code{RqcResultSet} objects created by
#' \code{\link{rqc}} and \code{\link{rqcQA}} functions.
#' @return Plot object from \code{\link{ggplot}} function.
#' @author Welliton Souza
#' @keywords graphics qc
#' @examples
#' 
#' checkpoint("Rqc", path=system.file(package="Rqc", "extdata"), {
#'   folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
#'   files <- list.files(full.names=TRUE, path=folder)
#'   rqcResultSet <- rqcQA(files, pair=c(1,1), workers=1)
#' }, keep="rqcResultSet")
#' rqcReadQualityPlot(rqcResultSet)
#' 
#' @export
rqcReadQualityPlot <- function(rqcResultSet)
{
    df <- rqcReadQualityCalc(rqcResultSet)
    ggplot(df, aes_string(y="quantile", x="value", colour="filename")) + 
        geom_line() + 
        labs(y="% of Reads Exceeding Quality (q)", x="Quality (q)", 
             colour="Filename")
}
