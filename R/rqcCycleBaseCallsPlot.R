rqcCycleBaseCallsPlot <- function(rqcResultSet)
{
    df <- rqcCycleBaseCallsCalc(rqcResultSet)
    len <- max(as.integer(df$cycle))
    ggplot(df, aes_string(x="cycle", y="value", fill="variable")) +
        geom_bar(stat="identity", width=1) + 
        scale_x_discrete(breaks=seq(from=1, to=len, by=len %/% 10)) + 
        labs(x="Cycle", y="%", fill="Base Call") +
        facet_wrap(facets=~filename, ncol=2) +
        scale_fill_manual(values=c("#009E73", "#0072B2", "#000000", "#D55E00", "#999999"))
}
