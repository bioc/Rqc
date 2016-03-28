#' Quality Assessment Rqc function
#' 
#' Process a set of files and returns a list of quality control data.  Files
#' must be FASTQ format, compressed or not.
#' 
#' Input files are read using \code{FastStreamer} and \code{FastSampler}
#' classes of \code{\link{ShortRead}} package.  Process multiple files in
#' parallel using \code{\link{bplapply}} function of \code{\link{BiocParallel}}
#' package.
#' 
#' @param x input file(s)
#' @param sample It reads a random sample from files if this parameter is TRUE.
#' @param n Number of sequences to read from each input file.  This represents
#' sample size if 'sample' parameter is TRUE, if not represents the chunk size
#' to read on each iteration.  Default is read a sample of one million
#' sequences from each input file.
#' @param group group name for each input file.
#' @param top number of top over-represented reads. Default is 10 reads.
#' @param pair combination of files for paired-end reads. By default, all input 
#' files are treated as single-end. For paired-end, please define a vector of
#' numbers where two index with the same value represent a pair. Examples, 
#' single-end \code{c(1,2,3,4)} and paired-end \code{c(1,1,2,2)}.
#' @param ... other parameters
#' @return A named list of \code{RqcResultSet} objects, each one represents a
#' file.
#' @author Welliton Souza
#' @seealso \code{\link{rqc}}
#' @examples
#' 
#' checkpoint("Rqc", path=system.file(package="Rqc", "extdata"), {
#'   folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
#'   files <- list.files(full.names=TRUE, path=folder)
#'   rqcResultSet <- rqcQA(files, pair=c(1,1), workers=1)
#' }, keep="rqcResultSet")
#' rqcReadQualityPlot(rqcResultSet)
#' 
#' @aliases rqcQA
#' @exportMethod rqcQA
setGeneric("rqcQA", 
    function(x, sample=TRUE, n=1e6, group=rep("None", length(x)), top=10, pair=seq_along(x), ...) 
        standardGeneric("rqcQA"),
    signature="x")

#' @describeIn rqcQA process a list of \code{\link[ShortRead]{FastqFile}} and
#' \code{\link[Rsamtools]{BamFile}} objects.
#' @inheritParams rqcQA
#' @param workers number of parallel workers
#' @exportMethod rqcQA
setMethod("rqcQA", signature(x="list"),
function(x, sample, n, group, top, pair, workers = multicoreWorkers())
{
    if (length(x) == 0)
        stop("Input files were not provided.")
    
    filesNotFound <- x[!file.exists(sapply(x, path))]
    if (length(filesNotFound) != 0) {
        for (f in filesNotFound) warning(paste(f, "not found."))
        stop("One or more input files were not found.")
    }
    if(length(group) != length(x)) 
        stop("'group' argument must have the same length of files argument.")
  
    param <- if (workers > 1) {
        MulticoreParam(workers,tasks=length(x),stop.on.error=TRUE)
    } else {
        SerialParam()
    }
 
    rqcResultSet <- bpmapply(rqcQA, x, sample, n, group, top, pair, 
        BPPARAM=param, SIMPLIFY=FALSE, USE.NAMES=FALSE)
  
  names(rqcResultSet) <- basename(sapply(x, path))
  return(rqcResultSet)
})

#' @describeIn rqcQA automatically detects file format 
#' (using \code{\link{detectFileFormat}} function) of input files then process.
#' @inheritParams rqcQA
#' @exportMethod rqcQA
setMethod("rqcQA", signature(x="character"),
function(x, sample = TRUE, n = 1e6, group = rep("None", length(x)),
         top=10, pair=seq_along(x), workers = multicoreWorkers())
{
    rqcQA(detectFileFormat(x), sample, n, group, top, pair, workers)
})

#' @describeIn rqcQA process only one BAM file.
#' @inheritParams rqcQA
#' @exportMethod rqcQA
setMethod("rqcQA", signature(x="BamFile"), function(x, sample, n, group, top, pair) {
    param <- ScanBamParam(what = c("seq", "qual"))
    yieldSize(x) <- n
    con <- open(x)

    if (sample) {
        yield <- function(a) readGAlignments(a, param=param)
        chunk <- mcols(reduceByYield(x, yield, c, REDUCEsampler(n)))
    } else {
        chunk <- scanBam(con, param = param)[[1]]
    }
    chunk <- ShortReadQ(chunk$seq, chunk$qual)
    
    readQuality <- .readQuality(chunk)
    readWidth <- .readWidth(chunk)
    readFrequency  <- .readFrequency(chunk)
    cycleQuality <- .cycleQuality(chunk)
    cycleBasecall <- .cycleBasecall(chunk)
    
    if (!sample) {
        chunk <- scanBam(con, param = param)[[1]]
        chunk <- ShortReadQ(chunk$seq, chunk$qual)
        while (length(chunk) != 0) {
            readQuality <- .mergeReadQuality(readQuality, .readQuality(chunk))
            readWidth <- .mergeReadWidth(readWidth, .readWidth(chunk))
            readFrequency <- .mergeReadFrequency(readFrequency, .readFrequency(chunk))
            cycleQuality <- .mergeCycleQuality(cycleQuality, .cycleQuality(chunk))
            cycleBasecall <- .mergeCycleBasecall(cycleBasecall, .cycleBasecall(chunk))
        
            chunk <- scanBam(con, param = param)[[1]]
            chunk <- ShortReadQ(chunk$seq, chunk$qual)
        }
    }
    close(con)
    
    fileInfo <- .fileInfo(x$path, group, "BAM", readWidth, pair)
    
    top <- readFrequency[order(readFrequency$count, decreasing = TRUE)[1:top], ]
    top$hash <- as.character(top$hash)
    rownames(top) <- NULL
    
    tbl <- table(readFrequency$count)
    count <- as.integer(tbl)
    occurrence <- as.integer(names(tbl))
    readFrequency <- data.frame(occurrence, count)
    
    lst = list(perFile=list(information=fileInfo, topReads=top),
               perCycle=list(quality=cycleQuality, baseCall=cycleBasecall),
               perRead=list(width=readWidth, averageQuality=readQuality, 
                            frequency=readFrequency))
    new("RqcResultSet", .srlist=lst)
})

#' @describeIn rqcQA process only one FASTQ file.
#' @inheritParams rqcQA
#' @exportMethod rqcQA
setMethod("rqcQA", signature(x="FastqFile"), function(x, sample, n, group, top, pair) {
    con <- if (sample) FastqSampler(x, n) else FastqStreamer(x$path, n)
    chunk <- yield(con)
    
    readQuality <- .readQuality(chunk)
    readWidth <- .readWidth(chunk)
    readFrequency  <- .readFrequency(chunk)
    cycleQuality <- .cycleQuality(chunk)
    cycleBasecall <- .cycleBasecall(chunk)
    
    if (!sample) {
        chunk <- yield(con)
        while (length(chunk) != 0) {
            readQuality <- .mergeReadQuality(readQuality, .readQuality(chunk))
            readWidth <- .mergeReadWidth(readWidth, .readWidth(chunk))
            readFrequency <- .mergeReadFrequency(readFrequency, .readFrequency(chunk))
            cycleQuality <- .mergeCycleQuality(cycleQuality, .cycleQuality(chunk))
            cycleBasecall <- .mergeCycleBasecall(cycleBasecall, .cycleBasecall(chunk))
        
            chunk <- yield(con)
        }
    }
    close(con)
    
    fileInfo <- .fileInfo(x$path, group, "FASTQ", readWidth, pair)
    
    top <- readFrequency[order(readFrequency$count, decreasing = TRUE)[1:top], ]
    top$hash <- as.character(top$hash)
    rownames(top) <- NULL
    
    tbl <- table(readFrequency$count)
    count <- as.integer(tbl)
    occurrence <- as.integer(names(tbl))
    readFrequency <- data.frame(occurrence, count)
    
    lst = list(perFile=list(information=fileInfo, topReads=top),
               perCycle=list(quality=cycleQuality, baseCall=cycleBasecall),
               perRead=list(width=readWidth, averageQuality=readQuality, 
                            frequency=readFrequency))
    new("RqcResultSet", .srlist=lst)
})
