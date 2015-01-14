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

.mergeTileQuality <- function(x, y)
{
    xy <- merge(x, y, c("lane", "tile"), all=TRUE, sort=FALSE)
    average <- rowMeans(xy[, -(1:2)], na.rm=TRUE)
    xy$average.x <- xy$average.y <- NULL
    xy$average <- average
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

.readQuality <- function(chunk)
{
    average <- alphabetScore(quality(chunk)) / width(chunk)
    tbl  <- table(average)
    count <- as.integer(tbl)
    average <- as.numeric(names(tbl))
    data.frame(average, count)
}

.tileQuality <- function(chunk)
{
    ids <- as.character(ShortRead::id(chunk))
    m <- regexec(":(\\d+):(\\d+):(\\d+):(\\d+)", ids)
    mat <- do.call(rbind, lapply(regmatches(ids, m), `[`, 2:5))
    class(mat) <- "integer"
    df <- data.frame(mat[, c(1,2)])
    df$lane <- df[[1]]
    df$tile <- df[[2]]
    df$average <- alphabetScore(quality(chunk)) / width(chunk)
    aggregate(average ~ lane + tile, df, mean)
}

.readWidth <- function(chunk)
{
    tbl <- table(width(chunk))
    count <- as.integer(tbl)
    width <- as.integer(names(tbl))
    data.frame(width, count)
}

.fileInfo <- function(file)
{
    path <- dirname(file)
    filename <- factor(basename(file))
    data.frame(filename, path, stringsAsFactors=FALSE)
}
