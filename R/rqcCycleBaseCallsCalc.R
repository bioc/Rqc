rqcCycleBaseCallsCalc <- function(rqcResultSet)
{
    f <- function(x) {
        variable <- x$base
        value <- x$count / sum(x$count) * 100
        data.frame(variable, value)
    }

    df <- perCycleBasecall(rqcResultSet)
    ddply(df, c("filename", "cycle"), f)
}
