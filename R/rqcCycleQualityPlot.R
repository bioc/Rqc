#' Per cycle quality plot
#' 
#' Creates a graph of per cycle quality.
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
#' rqcCycleQualityPlot(rqcResultSet)
#' 
#' @export
rqcCycleQualityPlot <- function(rqcResultSet)
{
    df <- rqcCycleQualityCalc(rqcResultSet)
    len <- max(as.integer(df$cycle))
    ggplot(df, aes_string(x="cycle", y="percentiles", fill="value")) + 
        geom_tile() +
        scale_fill_gradient(low='#EF8A62', high='#67A9CF', limits=c(0, 41)) + 
        facet_wrap(facets=~filename, ncol=2) +
        labs(x="Cycle", y="%", fill="Quality") +
        scale_x_discrete(breaks = seq(from = 1, to = len, by = len%/%10))
}
