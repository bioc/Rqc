#' Subset RqcResultSet object by group name.
#' 
#' This function subsets RqcResultSet object function by group name.
#' 
#' 
#' @param rqcResultSet list of \code{RqcResultSet} objects created by
#' \code{\link{rqc}} and \code{\link{rqcQA}} functions.
#' @param group Name of the group to subset
#' @return list of \code{RqcResultSet} objects from only one group.
#' @author Welliton Souza
#' @keywords qc
#' @examples
#' 
#' folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
#' files <- list.files(full.names=TRUE, path=folder)
#' rqcResultSet <- rqcQA(files, workers=1, group=c("a", "b"))
#' perFileInformation(subsetByGroup(rqcResultSet, "a"))
#' 
#' @export
subsetByGroup <- function(rqcResultSet, group)
{
    idx <- which(perFileInformation(rqcResultSet)$group %in% group)
    if (length(idx)) rqcResultSet[idx]
    else stop(paste0("Group '", group, "' not found on data."))
}
