#' Per read width plot
#' 
#' Creates bar graph of per read width from all elements of input list.
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
#' rqcReadWidthPlot(rqcResultSet)
#' 
#' @export
rqcReadWidthPlot <- function(rqcResultSet)
{
    df <- rqcReadWidthCalc(rqcResultSet)
    ggplot(df, aes_string(x="width", y="percentage")) +
        geom_bar(stat="identity") +
        facet_wrap(~filename, ncol = 2) +
        labs(x="Read Width", y="Proportion of reads (%)")
}
