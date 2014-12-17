rqcCycleQualityCalc <- function(rqcResultSet)
{
    probs <- seq(from=.01, to=.9, by=.01)
    bplapply(rqcResultSet, function(x) {
        df <- x[["perCycle"]][["quality"]]
        cycleQuality <- df[setdiff(names(df), c("cycle", "filename"))]
        score <- as.integer(names(cycleQuality))
        iqr <- apply(cycleQuality, 1, function(y) {
            quantile(Rle(score, y), probs)
        })
        iqr <- data.frame(iqr, percentiles=as.factor(probs * 100))
        iqr <- reshape2::melt(iqr, id.vars="percentiles", variable.name="cycle")
        iqr$cycle <- as.factor(as.integer(iqr$cycle))
        iqr$filename <- df$filename
        iqr
    })
}
