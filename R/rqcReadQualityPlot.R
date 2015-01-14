rqcReadQualityPlot <- function(rqcResultSet)
{
    df <- rqcReadQualityCalc(rqcResultSet)
    ggplot(df, aes_string(y="quantile", x="value", colour="filename")) + 
        geom_line() + 
        labs(y="% of Reads Exceeding Quality (q)", x="Quality (q)", 
             colour="Filename")
}
