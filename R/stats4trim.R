stats4trim <- function(rqcResultSet, qmin, pmax)
{
    if(missing(qmin) & missing(pmax)) {
        stop("Provide at least one filter criteria.")
    }
    
    average <- count <- NULL
    readQuality <- perReadQuality(rqcResultSet)
    df <- ddply(readQuality, "filename", summarize, percentage = 0:100, 
                quality = quantile(Rle(average, count), 0:100/100, names = FALSE))
    
    if (missing(qmin) & !missing(pmax)) {
        subset(df, percentage == pmax)
    } else if (!missing(qmin) & missing(pmax)) {
        ddply(df, "filename", function (x) head(subset(x, x$quality >= qmin), n=1))    
    } else {
        subset(df, percentage <= pmax & quality >= qmin)
    }
}