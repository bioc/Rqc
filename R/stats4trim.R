#' Minimun read mean quality and maximum percentage loss of reads estimations
#' for trimming step.
#' 
#' This function estimates how many reads would be lost if the sequences are
#' filtered by a minimum read mean quality value. Also this function estimates
#' what is the minimum read mean quality value for filtering and lose max
#' percentage defined.
#' 
#' 
#' @param rqcResultSet list of \code{RqcResultSet} objects created by
#' \code{\link{rqc}} and \code{\link{rqcQA}} functions.
#' @param qmin Minimum read mean quality value (bewteen 0 and 41).
#' @param pmax Maximum percentage of reads permitted been lost during trimming
#' step.
#' @return A data frame containg estimated minimum quality and maximum
#' percentage for each input file.
#' @author Welliton Souza
#' @keywords qc trimming
#' @examples
#' 
#' checkpoint("Rqc", path=system.file(package="Rqc", "extdata"), {
#'   folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
#'   files <- list.files(full.names=TRUE, path=folder)
#'   rqcResultSet <- rqcQA(files, pair=c(1,1), workers=1)
#' }, keep="rqcResultSet")
#' stats4trim(rqcResultSet, qmin=20)
#' stats4trim(rqcResultSet, pmax=10)
#' 
#' @export
stats4trim <- function(rqcResultSet, qmin, pmax)
{
    if(missing(qmin) & missing(pmax)) {
        stop("Provide at least one filter criteria.")
    }
    
    average <- count <- percentage <- NULL # avoid warnings
    readQuality <- perReadQuality(rqcResultSet)
    df <- ddply(readQuality, "filename", summarize, percentage = seq(0, 1, 0.0001) * 100, 
                quality = quantile(Rle(average, count), seq(0, 1, 0.0001), names = FALSE))
    
    if (missing(qmin) & !missing(pmax)) {
        subset(df, percentage == pmax)
    } else if (!missing(qmin) & missing(pmax)) {
        ddply(df, "filename", function (x) head(subset(x, x$quality >= qmin), n=1))    
    } else {
        subset(df, percentage <= pmax & quality >= qmin)
    }
}
