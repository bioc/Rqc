#' Class RqcResultSet
#'
#' @name RqcResultSet-class
#' @rdname RqcResultSet-class
#' @examples
#' checkpoint("Rqc", path=system.file(package="Rqc", "extdata"), {
#'   folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
#'   files <- list.files(full.names=TRUE, path=folder)
<<<<<<< HEAD
#'   rqcResultSet <- rqcQA(files, workers=1)
=======
#'   rqcResultSet <- rqcQA(files, pair=c(1,1), workers=1)
>>>>>>> master
#' }, keep="rqcResultSet")
#' head(perCycleBasecall(rqcResultSet))
#' head(perCycleQuality(rqcResultSet))
#' head(perReadFrequency(rqcResultSet))
#' head(perReadQuality(rqcResultSet))
#' head(perReadWidth(rqcResultSet))
#' perFileInformation(rqcResultSet)
#' perFileTopReads(rqcResultSet)
#' @exportClass RqcResultSet
setClass("RqcResultSet", contains=".QA")

#' Frequency distribution of cycle-specific base call
#' @rdname RqcResultSet-class
#' @param x \code{RqcResultSet} object or \code{list}
#' of \code{RqcResultSet} objects
#' @return data frame
#' @exportMethod perCycleBasecall
setGeneric("perCycleBasecall", function(x) 
    standardGeneric("perCycleBasecall"),
    signature="x")

#' @rdname RqcResultSet-class
#' @inheritParams perCycleBasecall
#' @exportMethod perCycleBasecall
setMethod("perCycleBasecall", signature(x="RqcResultSet"), function(x) {
    filename <- x[["perFile"]][["information"]]$filename
    cbind(x[["perCycle"]][["baseCall"]], filename)
})

#' @rdname RqcResultSet-class
#' @inheritParams perCycleBasecall
#' @exportMethod perCycleBasecall
setMethod("perCycleBasecall", signature(x="list"), function(x) {
    df.list <- lapply(x, perCycleBasecall)
    names(df.list) <- NULL
    do.call(rbind, df.list)
})

#' Frequency distribution of cycle-specific quality
#' @rdname RqcResultSet-class
#' @inheritParams perCycleBasecall
#' @return data frame
#' @exportMethod perCycleQuality
setGeneric("perCycleQuality", function(x) 
    standardGeneric("perCycleQuality"),
    signature="x")

#' @rdname RqcResultSet-class
#' @inheritParams perCycleBasecall
setMethod("perCycleQuality", signature(x="RqcResultSet"), function(x) {
    filename <- x[["perFile"]][["information"]]$filename
    group <- x[["perFile"]][["information"]]$group
    cbind(x[["perCycle"]][["quality"]], filename, group)
})

#' @rdname RqcResultSet-class
#' @inheritParams perCycleBasecall
setMethod("perCycleQuality", signature(x="list"), function(x) {
    df.list <- lapply(x, perCycleQuality)
    names(df.list) <- NULL
    do.call(rbind, df.list)
})

#' File information
#' @rdname RqcResultSet-class
#' @inheritParams perCycleBasecall
#' @return data frame
#' @exportMethod perFileInformation
setGeneric("perFileInformation", function(x) 
    standardGeneric("perFileInformation"),
    signature="x")

#' @rdname RqcResultSet-class
#' @inheritParams perCycleBasecall
setMethod("perFileInformation", signature(x="RqcResultSet"), function(x) {
    x[["perFile"]][["information"]]
})

#' @rdname RqcResultSet-class
#' @inheritParams perCycleBasecall
setMethod("perFileInformation", signature(x="list"), function(x) {
    df.list <- lapply(x, perFileInformation)
    names(df.list) <- NULL
    do.call(rbind, df.list)
})

#' Top over-represented sequencing reads
#' @rdname RqcResultSet-class
#' @inheritParams perCycleBasecall
#' @return data frame
#' @exportMethod perFileTopReads
setGeneric("perFileTopReads", function(x) 
    standardGeneric("perFileTopReads"),
    signature="x")

#' @rdname RqcResultSet-class
#' @inheritParams perCycleBasecall
setMethod("perFileTopReads", signature(x="RqcResultSet"), function(x) {
    filename <- x[["perFile"]][["information"]]$filename
    res <- cbind(x[["perFile"]][["topReads"]], filename)
    res$reads <- fromRRDNA(res$hash)
    res$hash <- NULL
    res
})

#' @rdname RqcResultSet-class
#' @inheritParams perCycleBasecall
setMethod("perFileTopReads", signature(x="list"), function(x) {
    df.list <- lapply(x, perFileTopReads)
    names(df.list) <- NULL
    do.call(rbind, df.list)
})

#' Read frequency table
#' @rdname RqcResultSet-class
#' @inheritParams perCycleBasecall
#' @return data frame
#' @exportMethod perReadFrequency
setGeneric("perReadFrequency", function(x) 
    standardGeneric("perReadFrequency"),
    signature="x")

#' @rdname RqcResultSet-class
#' @inheritParams perCycleBasecall
setMethod("perReadFrequency", signature(x="RqcResultSet"), function(x) {
    filename <- x[["perFile"]][["information"]]$filename
    cbind(x[["perRead"]][["frequency"]], filename)
})

#' @rdname RqcResultSet-class
#' @inheritParams perCycleBasecall
setMethod("perReadFrequency", signature(x="list"), function(x) {
    df.list <- lapply(x, perReadFrequency)
    names(df.list) <- NULL
    do.call(rbind, df.list)
})

#' Frequency distribution of per read mean quality
#' @rdname RqcResultSet-class
#' @inheritParams perCycleBasecall
#' @return data frame
#' @exportMethod perReadQuality
setGeneric("perReadQuality", function(x) 
    standardGeneric("perReadQuality"),
    signature="x")

#' @rdname RqcResultSet-class
#' @inheritParams perCycleBasecall
setMethod("perReadQuality", signature(x="RqcResultSet"), function(x) {
    filename <- x[["perFile"]][["information"]]$filename
    group <- x[["perFile"]][["information"]]$group
    cbind(x[["perRead"]][["averageQuality"]], filename, group)
})

#' @rdname RqcResultSet-class
#' @inheritParams perCycleBasecall
setMethod("perReadQuality", signature(x="list"), function(x) {
    df.list <- lapply(x, perReadQuality)
    names(df.list) <- NULL
    do.call(rbind, df.list)
})

#' Frequency distribution of read width
#' @rdname RqcResultSet-class
#' @inheritParams perCycleBasecall
#' @return data frame
#' @exportMethod perReadWidth
setGeneric("perReadWidth", function(x) 
    standardGeneric("perReadWidth"),
    signature="x")

#' @rdname RqcResultSet-class
#' @inheritParams perCycleBasecall
setMethod("perReadWidth", signature(x="RqcResultSet"), function(x) {
    filename <- x[["perFile"]][["information"]]$filename
    cbind(x[["perRead"]][["width"]], filename)
})

#' @rdname RqcResultSet-class
#' @inheritParams perCycleBasecall
setMethod("perReadWidth", signature(x="list"), function(x) {
    df.list <- lapply(x, perReadWidth)
    names(df.list) <- NULL
    do.call(rbind, df.list)
})
