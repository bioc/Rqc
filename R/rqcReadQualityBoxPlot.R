rqcReadQualityBoxPlot <- function(rqcResultSet)
{
    df <- rqcReadQualityBoxCalc(rqcResultSet)
    ggplot(df, aes_string(x="filename",ymin="ymin", lower="lower", 
                          middle="middle", upper="upper", ymax="ymax")) +
        geom_boxplot(stat = "identity") + 
        geom_point(aes(y=min)) + geom_point(aes(y=max)) +
        labs(x="Filename", y="Mean Quality") +
        coord_flip() + scale_x_discrete(limits=rev(levels(df$filename)))
}