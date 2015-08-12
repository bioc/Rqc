#' @describeIn rqcCycleBaseCallsPlot creates a line graph
#' @export
rqcCycleBaseCallsLinePlot <- function(rqcResultSet)
{
    df <- rqcCycleBaseCallsCalc(rqcResultSet)
    len <- max(as.integer(df$cycle))
    ggplot(df, aes_string(x="cycle", y="value", colour="variable")) + 
        geom_line(aes_string(group="variable")) +
        scale_x_discrete(breaks=seq(from=1, to=len, by=len %/% 10)) + 
        labs(x="Cycle", y="%", colour="Base Call") +
        facet_wrap(facets=~filename, ncol=2) +
        scale_colour_manual(values=getBioColor("DNA_BASES_N"))
}
