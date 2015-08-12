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
#'   rqcResultSet <- rqcQA(files, workers=1)
#' }, keep="rqcResultSet")
#' rqcReadQualityPlot(rqcResultSet)
#' 
#' @export
rqcReadQualityPlot <- function(rqcResultSet)
{
    size <- length(rqcResultSet)
    if (size > 11) 
        warning("Number of samples is greater than 11, the color of the lines will be repeated.")
    if (size < 3)
        size <- 3
    
    df <- rqcReadQualityCalc(rqcResultSet)
    ggplot(df, aes_string(y="quantile", x="value", colour="filename")) + 
        geom_line() + 
        labs(y="% of Reads Exceeding Quality (q)", x="Quality (q)", 
             colour="Filename") +
        scale_colour_manual(values=colorBlindSafePal("RdYlBu")(size, TRUE))
}
