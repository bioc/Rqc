#' Read frequency plot
#' 
#' This function creates a bar graph of read frequency (in percentage).
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
#' rqcReadFrequencyPlot(rqcResultSet)
#' 
#' @export
rqcReadFrequencyPlot <- function(rqcResultSet)
{
    df <- rqcReadFrequencyCalc(rqcResultSet)
    p <- ggplot(df, aes_string(x="occurrence", y="percentage"))
    if (max(df$occurrence) < 10) {
      p + geom_bar(aes_string(fill="filename"), stat = "identity", position="dodge") +
        labs(x="Number of occurences", y="Proportion of reads (%)", fill="Filename")
    } else {
      p + geom_line(aes_string(colour="filename")) + scale_x_log10() +
        labs(x="Number of occurences", y="Proportion of reads (%)", colour="Filename")
    }
}
