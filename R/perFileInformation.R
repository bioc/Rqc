perFileInformation <- function(rqcResultSet)
{
    f <- function(x) x[["perFile"]][["information"]]

    if (is(rqcResultSet, "RqcResultSet")) return(f(rqcResultSet))
    df.list <- lapply(rqcResultSet, f)
    names(df.list) <- NULL
    do.call(rbind, df.list)
}
