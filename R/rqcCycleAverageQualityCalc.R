rqcCycleAverageQualityCalc <- function(rqcResultSet)
{
    dfList <- bplapply(rqcResultSet, function(x) {
        df <- x[["perCycle"]][["quality"]]
        cycleQuality <- df[setdiff(names(df), c("cycle", "filename"))]
        score <- as.integer(names(cycleQuality))
        quality <- apply(cycleQuality, 1, function(y) mean(Rle(score, y)))
        data.frame(quality, cycle=df$cycle, filename=df$filename)
    })
    do.call(rbind, dfList)
}