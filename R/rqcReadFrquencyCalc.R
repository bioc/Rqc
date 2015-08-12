#' @describeIn rqcReadFrequencyPlot calculates necessary statistics
#' @export
rqcReadFrequencyCalc <- function(rqcResultSet)
{
    df <- perReadFrequency(rqcResultSet)
    total <- perFileInformation(rqcResultSet)[, c("filename", "reads")]
    df <- merge(df, total)
    df$count <- df$count * df$occurrence
    df$percentage <- df$count / df$reads * 100
    df
}
