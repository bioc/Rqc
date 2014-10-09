rqcCycleQualityPlot <- function(rqcResultSet)
{
    dfList <- rqcCycleQualityCalc(rqcResultSet)
    df <- do.call(rbind, dfList)
    len <- max(as.integer(df$cycle))
    ggplot(df, aes_string(x="cycle", y="percentiles", fill="value")) + 
        geom_bar(stat = "identity") +
        scale_fill_gradient(low='#EF8A62', high='#67A9CF') + 
        facet_wrap(facets=~filename, ncol=2) +
        labs(x="Cycle", y="%", fill="Quality") +
        scale_x_discrete(breaks=as.integer(seq(from=1, to=len, length=10)))
}
