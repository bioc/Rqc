#' @describeIn rqcCycleQualityPlot calculates necessary statistics
#' @export
rqcCycleQualityCalc <- function(rqcResultSet)
{
    f <- function(x) {
        percentiles <- 1:100
        value <- quantile(Rle(x$score, x$count), seq(0.01, 1, 0.01), names=FALSE)
        data.frame(percentiles, value)
    }

    df <- perCycleQuality(rqcResultSet)
    ddply(df, c("filename", "cycle"), f)
}
