#' @describeIn rqcCycleQualityBoxPlot calculates necessary statistics
#' @export
rqcCycleQualityBoxCalc <- function(rqcResultSet)
{
    probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
    f <- function(x) {
        quantile(Rle(x$score, x$count), probs, names = FALSE)
    }

    df <-perCycleQuality(rqcResultSet)
    res <- ddply(df, c("filename", "cycle"), f)
    names(res)[c(-1,-2)] <- c("ymin", "lower", "middle", "upper", "ymax")
    res
}
