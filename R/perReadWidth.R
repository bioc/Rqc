perReadWidth <- function(rqcResultSet)
{
    f <- function(x) {
        filename <- x[["perFile"]][["information"]]$filename
        cbind(x[["perRead"]][["width"]], filename)
    }

    if (is(rqcResultSet, "RqcResultSet")) return(f(rqcResultSet))
    df.list <- lapply(rqcResultSet, f)
    names(df.list) <- NULL
    do.call(rbind, df.list)
}
