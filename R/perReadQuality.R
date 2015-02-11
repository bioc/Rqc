perReadQuality <- function(rqcResultSet)
{
    f <- function(x) {
        filename <- x[["perFile"]][["information"]]$filename
        group <- x[["perFile"]][["information"]]$group
        cbind(x[["perRead"]][["averageQuality"]], filename, group)
    }

    if (is(rqcResultSet, "RqcResultSet")) return(f(rqcResultSet))
    df.list <- lapply(rqcResultSet, f)
    names(df.list) <- NULL
    do.call(rbind, df.list)
}
