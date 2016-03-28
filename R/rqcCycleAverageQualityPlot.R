#' Per cycle average quality by files
#' 
#' This function plots line graph of per cycle average quality.
#' 
#' @param rqcResultSet list of \code{RqcResultSet} objects created by
#' \code{\link{rqc}} and \code{\link{rqcQA}} functions.
#' @author Welliton Souza
#' @seealso \code{\link{rqcGroupCycleAverageQualityPlot}} plots cycle-specific quality by groups
#' @keywords graphics qc
#' @return ggplot2 object
#' @examples
#' checkpoint("Rqc", path=system.file(package="Rqc", "extdata"), {
#'   folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
#'   files <- list.files(full.names=TRUE, path=folder)
#'   rqcResultSet <- rqcQA(files, pair=c(1,1), workers=1)
#' }, keep="rqcResultSet")
#' rqcCycleAverageQualityPlot(rqcResultSet)
#' @export
rqcCycleAverageQualityPlot <- function(rqcResultSet)
{
    df <- rqcCycleAverageQualityCalc(rqcResultSet)
    len <- max(as.integer(df$cycle))
    ggplot(df, aes_string(x="cycle", y="quality", colour="filename")) +
        geom_point() + geom_line(aes_string(group="filename")) +
        labs(x="Cycle", y="Average Quality", colour="Filename") +
        scale_x_discrete(breaks=seq(from=1, to=len, by=len %/% 20)) 
}
