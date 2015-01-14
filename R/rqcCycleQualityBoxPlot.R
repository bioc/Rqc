rqcCycleQualityBoxPlot <- function(rqcResultSet)
{
    df <- rqcCycleQualityBoxCalc(rqcResultSet)
    len <- max(as.integer(df$cycle))
    ggplot(df, aes_string(x="cycle", ymin="ymin", lower="lower", 
                          middle="middle", upper="upper", ymax="ymax")) + 
        geom_boxplot(stat="identity") + 
        facet_wrap(facets=~filename, ncol=2) +
        scale_x_discrete(breaks=seq(from=1, to=len, by=len %/% 10)) + 
        labs(x="Cycle", y="Quality")
}
