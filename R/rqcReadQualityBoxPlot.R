#' Per read mean quality box plot
#' 
#' This function creates crate a graphic charts with box plots describing per
#' read mean quality distribution for each input file
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
<<<<<<< HEAD
#'   rqcResultSet <- rqcQA(files, workers=1)
=======
#'   rqcResultSet <- rqcQA(files, pair=c(1,1), workers=1)
>>>>>>> master
#' }, keep="rqcResultSet")
#' rqcReadQualityBoxPlot(rqcResultSet)
#' 
#' @export
rqcReadQualityBoxPlot <- function(rqcResultSet)
{
    size <- length(unique(perFileInformation(rqcResultSet)$group))
    if (size > 11) 
        warning("Number of groups is greater than 11, the color of the lines will be repeated.")
    if (size < 3)
        size <- 3
    
    df <- rqcReadQualityBoxCalc(rqcResultSet)
    ggplot(df, aes_string(x="filename",ymin="ymin", lower="lower", 
                          middle="middle", upper="upper", ymax="ymax")) +
        geom_boxplot(stat = "identity", aes_string(fill="group")) + 
        geom_point(aes(y=min)) + geom_point(aes(y=max)) +
        labs(x="Filename", y="Mean Quality", fill="Group") +
<<<<<<< HEAD
        coord_flip() + scale_x_discrete(limits=rev(levels(df$filename))) +
        scale_fill_manual(values=colorBlindSafePal("RdYlBu")(size, TRUE))
=======
        coord_flip() + scale_x_discrete(limits=rev(levels(df$filename)))
>>>>>>> master
}
