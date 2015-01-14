rqcCycleGCCalc <- function(rqcResultSet)
{
    f <- function(x) {
        gc <- sum(x$count[c(2,3)]) / sum(x$count) * 100
        data.frame(gc)
    }
    
    df <- perCycleBasecall(rqcResultSet)
    ddply(df, c("filename", "cycle"), f)
}
