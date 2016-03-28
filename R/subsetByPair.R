#' Subset RqcResultSet object by pair files.
#' 
#' This function subsets RqcResultSet object function by pair files.
#' 
#' 
#' @param rqcResultSet list of \code{RqcResultSet} objects created by
#' \code{\link{rqc}} and \code{\link{rqcQA}} functions.
#' @param pair index of the pair
#' @return list of \code{RqcResultSet} objects from only one pair.
#' @author Welliton Souza
#' @keywords qc
#' @examples
#' 
#' checkpoint("Rqc", path=system.file(package="Rqc", "extdata"), {
#'   folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
#'   files <- list.files(full.names=TRUE, path=folder)
#'   rqcResultSet <- rqcQA(files, pair=c(1,1), workers=1)
#' }, keep="rqcResultSet")
#' perFileInformation(subsetByPair(rqcResultSet, 1))
#' 
#' @export
subsetByPair <- function(rqcResultSet, pair)
{
    idx <- which(perFileInformation(rqcResultSet)$pair %in% pair)
    if (length(idx)) rqcResultSet[idx]
    else stop(paste0("Pair ", pair, " not found on data."))
}
