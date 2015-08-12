#' @describeIn rqcGroupCycleAverageQualityPlot calculates necessary statistics
#' @export
rqcGroupCycleAverageQualityCalc <- function(rqcResultSet)
{
    df <- rqcCycleAverageQualityCalc(rqcResultSet)
    ddply(df, c("group", "cycle"), function(x) data.frame(quality = mean(x$quality)))
}