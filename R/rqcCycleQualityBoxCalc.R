rqcCycleQualityBoxCalc <- function(rqcResultSet)
{
    probs <- c(.10, .25, .50, .75, .90)
    bplapply(rqcResultSet, function(x) {
        df <- x[["perCycle"]][["quality"]]
        cycleQuality <- df[setdiff(names(df), c("cycle", "filename"))]
        score <- as.integer(names(cycleQuality))
        iqr <- apply(cycleQuality, 1, function(y) quantile(Rle(score, y), probs))
        rownames(iqr) <- c("ymin", "lower", "middle", "upper", "ymax")
        data.frame(t(iqr), cycle=factor(df$cycle), filename=df$filename)
    })
}
