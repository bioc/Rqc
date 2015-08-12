#' @describeIn rqcReadQualityPlot calculates necessary statistics
#' @export
rqcReadQualityCalc <- function(rqcResultSet)
{
    f <- function(x) {
        quantile <- 100 - seq(0, 100, 1)
        value <- quantile(Rle(x$average, x$count), seq(0, 1, 0.01), names=FALSE)
        data.frame(quantile, value)
    }

    df <- perReadQuality(rqcResultSet)
    ddply(df, "filename", f)
}
