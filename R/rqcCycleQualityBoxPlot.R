#' Per cycle quality box plot
#' 
#' Plots per cycle quality box plot.
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
#' rqcCycleQualityBoxPlot(rqcResultSet)
#' 
#' @export
rqcCycleQualityBoxPlot <- function(rqcResultSet)
{
    df <- rqcCycleQualityBoxCalc(rqcResultSet)
    len <- max(as.integer(df$cycle))
    ggplot(df, aes_string(x="cycle", ymin="ymin", lower="lower", 
                          middle="middle", upper="upper", ymax="ymax")) + 
        geom_boxplot(stat="identity") + 
        facet_wrap(facets=~filename, ncol=2) +
        scale_x_discrete(breaks=seq(from=1, to=len, by=len %/% 10)) + 
        labs(x="Cycle", y="Quality")
}
