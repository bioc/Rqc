rqcReadQualityCalc <- function(rqcResultSet)
{
    average <- bplapply(rqcResultSet, function(x) {
        quantile(x[["perRead"]][["averageQuality"]]$average, seq(0, 1, .01))
    })
    average <- data.frame(average, quantile=100-seq(0, 100, 1))
    reshape2::melt(average, id.vars="quantile", variable.name="filename")
}