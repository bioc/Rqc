#' Per group average quality across cycles
#' 
#' This function plots cycle-specific quality by groups
#' 
#' @param rqcResultSet list of \code{RqcResultSet} objects created by \code{\link{rqc}} and \code{\link{rqcQA}} functions.
#' @author Welliton Souza
#' @seealso \code{\link{rqcCycleAverageQualityPlot}} plots cycle-specific quality by files
#' @keywords graphics qc
#' @return ggplot2 object
#' @examples
#' checkpoint("Rqc", path=system.file(package="Rqc", "extdata"), {
#'   folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
#'   files <- list.files(full.names=TRUE, path=folder)
#'   rqcResultSet <- rqcQA(files, pair=c(1,1), workers=1)
#' }, keep="rqcResultSet")
#' rqcGroupCycleAverageQualityPlot(rqcResultSet)
#' @export
rqcGroupCycleAverageQualityPlot <- function(rqcResultSet)
{
    df <- rqcGroupCycleAverageQualityCalc(rqcResultSet)
    len <- max(as.integer(df$cycle))
    ggplot(df, aes_string(x="cycle", y="quality", colour="group")) +
        geom_point() + geom_line(aes_string(group="group")) +
        labs(x="Cycle", y="Average Quality", colour="Group") +
        scale_x_discrete(breaks=seq(from=1, to=len, by=len %/% 20))
}