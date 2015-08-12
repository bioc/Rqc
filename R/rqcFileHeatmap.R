#' Heatmap of distance matrix of top over-represented reads
#' 
#' This function plots a heatmap of distance matrix of top over-represented
#' reads. This function does not work with list of RqcResultSet objects, only
#' with one RqcResultSet object.
#' 
#' 
#' @param rqcResultSet \code{RqcResultSet} object created by \code{\link{rqc}}
#' and \code{\link{rqcQA}} functions.
#' @return Plot object
#' @author Welliton Souza
#' @keywords qc graphics
#' @examples
#' 
#' checkpoint("Rqc", path=system.file(package="Rqc", "extdata"), {
#'   folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
#'   files <- list.files(full.names=TRUE, path=folder)
#'   rqcResultSet <- rqcQA(files, workers=1)
#' }, keep="rqcResultSet")
#' rqcFileHeatmap(rqcResultSet[[1]])
#' 
#' @export
rqcFileHeatmap <- function(rqcResultSet)
{
    if (!is(rqcResultSet, "RqcResultSet"))
        stop("'perFileHeatmap' function only works with RqcResultSet object.")
    hash <- rqcResultSet[["perFile"]][["topReads"]]$hash
    heatmap(matdist(hash))
}
