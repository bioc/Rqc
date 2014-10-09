rqcReadWidthPlot <- function(rqcResultSet)
{
    df <- rqcReadWidthCalc(rqcResultSet)
    ggplot(df, aes_string(x="width", y="count")) + 
        geom_bar(width=.5, stat="identity", position="dodge") +
        facet_wrap(facets=~filename, ncol=2) +
        labs(x="Read Width", y="Frequency")
}
