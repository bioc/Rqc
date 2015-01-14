rqcCycleAverageQualityPlot <- function(rqcResultSet)
{
    df <- rqcCycleAverageQualityCalc(rqcResultSet)
    len <- max(as.integer(df$cycle))
    ggplot(df, aes_string(x="cycle", y="quality", colour="filename")) +
        geom_point() + geom_line(aes_string(group="filename")) +
        labs(x="Cycle", y="Average Quality", colour="Filename") +
        scale_x_discrete(breaks=seq(from=1, to=len, by=len %/% 20))
}