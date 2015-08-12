.mergeReadWidth <- function(x, y)
{
    xy <- merge(x, y, "width", all=TRUE)
    count <- rowSums(xy[, -1], na.rm=TRUE)
    xy$count.x <- xy$count.y <- NULL
    xy$count <- count
    xy
}

.mergeReadQuality <- function(x, y)
{
    xy <- merge(x, y, "average", all=TRUE)
    count <- rowSums(xy[, -1], na.rm=TRUE)
    xy$count.x <- xy$count.y <- NULL
    xy$count <- count
    xy
}

.mergeCycleQuality <- function(x, y)
{
    xy <-  merge(x, y, c("cycle", "quality", "score"), all=TRUE, sort=FALSE)
    count <- rowSums(xy[, -(1:3)], na.rm=TRUE)
    xy$count.x <- xy$count.y <- NULL
    xy$count <- count
    xy
}

.mergeCycleBasecall <- function(x, y)
{
    xy <- merge(x, y, c("cycle", "base"), all=TRUE, sort=FALSE)
    count <- rowSums(xy[, -(1:2)], na.rm=TRUE)
    xy$count.x <- xy$count.y <- NULL
    xy$count <- count
    xy
}

.mergeReadFrequency <- function(x, y)
{
    xy <- merge(x, y, "hash", all=TRUE, sort=TRUE)
    count <- rowSums(xy[, -1], na.rm=TRUE)
    xy$count.x <- xy$count.y <- NULL
    xy$count <- count
    xy
}

.cycleBasecall <- function(chunk)
{
    bases <- c("A", "C", "G", "T", "N")
    cycleBasecall <- alphabetByCycle(sread(chunk))[bases, ]
    cycles <- factor(seq_len(ncol(cycleBasecall)))
    cycle <- rep(cycles, rep(length(bases), length(cycles)))
    base <- factor(bases, levels = bases)
    count <- as.vector(cycleBasecall)
    data.frame(cycle, base, count)
}

.cycleQuality <- function(chunk)
{
    chunkQuality <- quality(chunk)
    score <- encoding(chunkQuality)
    mat <- alphabetByCycle(chunkQuality, names(score))
    cycles <- factor(seq_len(ncol(mat)))
    cycle <- rep(cycles, rep(length(score), length(cycles)))
    quality <- names(score)
    score <- as.integer(score)
    count  <- as.vector(mat)
    data.frame(cycle, quality, score, count)
}

.readMeanQuality <- function(chunk)
{
    alphabetScore(quality(chunk)) / width(chunk)
}

.readQuality <- function(chunk)
{
    average <- .readMeanQuality(chunk)
    tbl  <- table(average)
    count <- as.integer(tbl)
    average <- as.numeric(names(tbl))
    data.frame(average, count)
}

.readWidth <- function(chunk)
{
    tbl <- table(width(chunk))
    count <- as.integer(tbl)
    width <- as.integer(names(tbl))
    data.frame(width, count)
}

.fileInfo <- function(file, group, format, readWidth)
{
    path <- dirname(file)
    filename <- factor(basename(file))
    reads <- sum(readWidth$count)
    data.frame(filename, format, group, reads, path, stringsAsFactors=FALSE)
}

.readFrequency <- function(chunk)
{
    tbl <- table(toRRDNA(as.character(sread(chunk))))
    count <- as.integer(tbl)
    hash <- names(tbl)
    data.frame(hash, count, stringsAsFactors = FALSE)
}