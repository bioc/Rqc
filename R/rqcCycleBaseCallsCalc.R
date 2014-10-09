rqcCycleBaseCallsCalc <- function(rqcResultSet)
{
    f <- function(x) {
        x / sum(x) * 100
    }
    
    lapply(rqcResultSet, function(x) {
        baseCall <- x[["perCycle"]][["baseCall"]]
        baseAverage <- apply(baseCall[, c("A","C","G","T","N")], 1, f)
        df <- data.frame(t(baseAverage),
                         cycle=factor(baseCall$cycle),
                         filename=baseCall$filename)
        melt(df, id.vars=c("filename", "cycle"))
    })
}