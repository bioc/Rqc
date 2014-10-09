rqcReadWidthCalc <- function(rqcResultSet)
{
    dfList <- lapply(rqcResultSet, function(x) {
        df <- x[["perRead"]][["width"]]
        df$width <- factor(df$width)
        df$count <- factor(df$count)
        df
    })
    do.call(rbind, dfList)
}
