rqcReadWidthPlot <- function(rqcResultSet)
{
    df <- rqcReadWidthCalc(rqcResultSet)
    ggplot(df, aes_string(x="width", weight="count")) +
        geom_histogram(binwidth=1) +
        facet_wrap(~filename, ncol = 2) +
        labs(x="Read Width", y="Frequency")
}
