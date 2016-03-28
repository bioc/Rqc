#' Per cycle percentual GC plot
#' 
#' Creates a line graph of per cycle percentual GC.
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
#' rqcCycleGCPlot(rqcResultSet)
#' 
#' @export
rqcCycleGCPlot <- function(rqcResultSet)
{
    df <- rqcCycleGCCalc(rqcResultSet)
    len <- max(as.integer(df$cycle))
    ggplot(df, aes_string(x="cycle", y="gc", colour="filename")) +
        geom_line(aes_string(group="filename")) +
        scale_x_discrete(breaks=seq(from=1, to=len, by=len %/% 20)) +
        labs(x="Cycle", y="% GC", colour="Filename")
}
