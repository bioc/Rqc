#' @describeIn rqcReadWidthPlot calculates necessary statistics
#' @export
rqcReadWidthCalc <- function(rqcResultSet)
{
    df <- perReadWidth(rqcResultSet)
    total <- perFileInformation(rqcResultSet)[, c("filename", "reads")]
    df <- merge(df, total)
    transform(df, width = factor(df$width), percentage=df$count / df$reads * 100)
}
