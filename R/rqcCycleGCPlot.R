rqcCycleGCPlot <- function(rqcResultSet)
{
    df <- rqcCycleGCCalc(rqcResultSet)
    len <- max(as.integer(df$cycle))
    ggplot(df, aes_string(x="cycle", y="gc", colour="filename")) +
        geom_line(aes_string(group="filename")) +
        scale_x_discrete(breaks=seq(from=1, to=len, by=len %/% 20)) +
        labs(x="Cycle", y="% GC", colour="Filename")
}
