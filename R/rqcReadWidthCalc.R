rqcReadWidthCalc <- function(rqcResultSet)
{
    df <- perReadWidth(rqcResultSet)
    transform(df, width = factor(df$width))
}
