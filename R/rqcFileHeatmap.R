#' Heatmap of distance matrix of top over-represented reads
#' 
#' This function plots a heatmap of distance matrix of top over-represented
#' reads. This function does not work with list of RqcResultSet objects, only
#' with one RqcResultSet object.
#' 
#' 
#' @param rqcResultSet \code{RqcResultSet} object created by \code{\link{rqc}}
#' and \code{\link{rqcQA}} functions.
<<<<<<< HEAD
#' @return Plot object
=======
#' @param dist.method the distance measure to be used by \code{\link{dist}} function.
#' @param hclust.method the agglomeration method to be used by \code{\link{hclust}} function.
#' @return Plot object from \code{\link{ggplot}} function.
>>>>>>> master
#' @author Welliton Souza
#' @keywords qc graphics
#' @examples
#' 
#' checkpoint("Rqc", path=system.file(package="Rqc", "extdata"), {
#'   folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
#'   files <- list.files(full.names=TRUE, path=folder)
<<<<<<< HEAD
#'   rqcResultSet <- rqcQA(files, workers=1)
=======
#'   rqcResultSet <- rqcQA(files, pair=c(1,1), workers=1)
>>>>>>> master
#' }, keep="rqcResultSet")
#' rqcFileHeatmap(rqcResultSet[[1]])
#' 
#' @export
<<<<<<< HEAD
rqcFileHeatmap <- function(rqcResultSet)
{
    if (!is(rqcResultSet, "RqcResultSet"))
        stop("'perFileHeatmap' function only works with RqcResultSet object.")
    hash <- rqcResultSet[["perFile"]][["topReads"]]$hash
    heatmap(matdist(hash))
=======
rqcFileHeatmap <- function(rqcResultSet, dist.method="euclidean", hclust.method="ward.D")
{
    if (!is(rqcResultSet, "RqcResultSet"))
        stop("'perFileHeatmap' function only works with RqcResultSet object.")
    topReads <- rqcResultSet[["perFile"]][["topReads"]]
    hash <- topReads$hash
    similar <- ((nchar(hash) * 3) - matdist(hash)) / (nchar(hash) * 3) * 100
    dist <- dist(similar, method=dist.method)
    order <- hclust(dist, method=hclust.method)$order
    similar.melt <- melt(similar)
    similar.melt$Var1 <- factor(similar.melt$Var1, levels = order)
    similar.melt$Var2 <- factor(similar.melt$Var2, levels = order)
    ggplot(similar.melt, aes_string(x="Var1", y="Var2", fill="value")) + 
        geom_tile(colour = "white") +
        scale_fill_continuous(limits=c(0, 100), low="white", high="steelblue") +
        labs(x = "Top overrepresented reads", y = "Top overrepresented reads",
             fill = "Similarity (%)", title=perFileInformation(rqcResultSet)$filename)
>>>>>>> master
}
