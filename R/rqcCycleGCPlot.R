rqcCycleGCPlot <- function(rqcResultSet)
{
    df <- rqcCycleGCCalc(rqcResultSet)
    len <- max(df$cycle)
    ggplot(df, aes_string(x="cycle", y="gc", colour="filename")) +
        geom_line(size=1) +
        scale_x_discrete(breaks=seq(from=1, to=len, by=len %/% 20)) +
        labs(x="Cycle", y="% GC", colour="Filename")
}
