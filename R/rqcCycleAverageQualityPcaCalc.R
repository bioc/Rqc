#' @describeIn rqcCycleAverageQualityPcaPlot calculates necessary statistics
#' @export
rqcCycleAverageQualityPcaCalc <- function(rqcResultSet)
{
  df <- rqcCycleAverageQualityCalc(rqcResultSet)
  df.wide <- dcast(df, cycle ~ filename, value.var = "quality")
  fit <- prcomp(~ ., data=df.wide[,-1])
  return(fit)
}
