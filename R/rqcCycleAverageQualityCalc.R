rqcCycleAverageQualityCalc <- function(rqcResultSet)
{
    f <- function(x) {
        quality <- mean(Rle(x$score, x$count))
        data.frame(quality)
    }
    
    df <- perCycleQuality(rqcResultSet)
    ddply(df, c("filename", "cycle"), f)
}
