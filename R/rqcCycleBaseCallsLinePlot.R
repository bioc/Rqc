rqcCycleBaseCallsLinePlot <- function(rqcResultSet)
{
    dfList <- rqcCycleBaseCallsCalc(rqcResultSet)
    df <- do.call(rbind, dfList)
    len <- max(as.integer(df$cycle))
    ggplot(df, aes_string(x="cycle", y="value", colour="variable", group="variable")) + 
        geom_line() +
        scale_x_discrete(breaks=seq(from=1, to=len, by=len %/% 20)) + 
        labs(x="Cycle", y="%", colour="Base Call") +
        facet_wrap(facets=~filename, ncol=2) +
        scale_colour_manual(values=c("#009E73", "#0072B2", "#000000", 
                                     "#D55E00", "#999999"))
}