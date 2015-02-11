stats4trim <- function(rqcResultSet, qmin, pmax)
{
    if(missing(qmin) & missing(pmax)) {
        stop("Provide at least one filter criteria.")
    }
    
    average <- count <- percentage <- NULL # avoid warnings
    readQuality <- perReadQuality(rqcResultSet)
    df <- ddply(readQuality, "filename", summarize, percentage = seq(0, 1, 0.0001) * 100, 
                quality = quantile(Rle(average, count), seq(0, 1, 0.0001), names = FALSE))
    
    if (missing(qmin) & !missing(pmax)) {
        subset(df, percentage == pmax)
    } else if (!missing(qmin) & missing(pmax)) {
        ddply(df, "filename", function (x) head(subset(x, x$quality >= qmin), n=1))    
    } else {
        subset(df, percentage <= pmax & quality >= qmin)
    }
}