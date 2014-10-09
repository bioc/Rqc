rqcCycleAverageQualityPlot <- function(rqcResultSet)
{
    df <- rqcCycleAverageQualityCalc(rqcResultSet)
    len <- max(df$cycle)
    ggplot(df, aes_string(x="cycle", y="quality", colour="filename")) +
        geom_line() + geom_point() +
        labs(x="Cycle", y="Average Quality", colour="Filename") +
        scale_x_discrete(breaks=seq(from=1, to=len, by=len %/% 20))
}