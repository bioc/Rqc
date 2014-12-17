rqcCycleQualityPlot <- function(rqcResultSet)
{
    dfList <- rqcCycleQualityCalc(rqcResultSet)
    df <- do.call(rbind, dfList)
    len <- max(as.integer(df$cycle))
    ggplot(df, aes_string(x="cycle", y="percentiles", fill="value")) + 
        geom_tile() +
        scale_fill_gradient(low='#EF8A62', high='#67A9CF', limits=c(0, 40)) + 
        facet_wrap(facets=~filename, ncol=2) +
        labs(x="Cycle", y="%", fill="Quality") +
        scale_x_discrete(breaks=as.integer(seq(from=1, to=len, length=10))) +
        scale_y_discrete(breaks=c(0, 25, 50, 75))
}
