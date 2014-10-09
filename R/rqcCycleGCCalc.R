rqcCycleGCCalc <- function(rqcResultSet)
{
    f <- function(x) {
        sum(x[c("C", "G")]) / sum(x) * 100
    }
    
    gcList <- lapply(rqcResultSet, function(x) {
        baseCall <- x[["perCycle"]][["baseCall"]]
        gc <- apply(baseCall[, c("A","C","G","T","N")], 1, f)
        data.frame(gc=gc, cycle=baseCall$cycle, 
                   filename=baseCall$filename)
    })
    do.call(rbind, gcList)
}