rqcCycleQualityPlot <- function(rqcResultSet)
{
    df <- rqcCycleQualityCalc(rqcResultSet)
    len <- max(as.integer(df$cycle))
    ggplot(df, aes_string(x="cycle", y="percentiles", fill="value")) + 
        geom_tile() +
        scale_fill_gradient(low='#EF8A62', high='#67A9CF', limits=c(0, 41)) + 
        facet_wrap(facets=~filename, ncol=2) +
        labs(x="Cycle", y="%", fill="Quality") +
        scale_x_discrete(breaks = seq(from = 1, to = len, by = len%/%10))
}