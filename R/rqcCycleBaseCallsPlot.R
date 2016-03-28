#' Per cycle base calls plot
#' 
#' Creates a bar graph of per cycle base calls.
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
#' rqcCycleBaseCallsPlot(rqcResultSet)
#' 
#' @export
rqcCycleBaseCallsPlot <- function(rqcResultSet)
{
    df <- rqcCycleBaseCallsCalc(rqcResultSet)
    len <- max(as.integer(df$cycle))
    ggplot(df, aes_string(x="cycle", y="value", fill="variable")) +
        geom_bar(stat="identity", width=1) + 
        scale_x_discrete(breaks=seq(from=1, to=len, by=len %/% 10)) + 
        labs(x="Cycle", y="%", fill="Base Call") +
        facet_wrap(facets=~filename, ncol=2) +
        scale_fill_manual(values=getBioColor("DNA_BASES_N"))
}
