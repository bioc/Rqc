rqcQA <- function(files, sample = TRUE, n = 1e6) 
{
    rqcResultSet <- if (sample) {
        bplapply(files, function(file) {
            filename <- basename(file)
            sampler <- FastqSampler(file, n=n)
            chunk <- yield(sampler)
            close(sampler)
            chunkQuality <- quality(chunk)
            chunkBaseCall <- sread(chunk)
            chunkWidth <- width(chunk)
            enc <- encoding(chunkQuality)
            readAverageQuality <- alphabetScore(chunkQuality) / chunkWidth
            readWidth <- data.frame(table(chunkWidth))
            cycleQuality <- alphabetByCycle(chunkQuality)
            cycleBaseCall <- alphabetByCycle(chunkBaseCall)
            .RqcResultSet(enc, filename, readAverageQuality, readWidth, 
                          cycleQuality, cycleBaseCall)
        })
    } else {
        bplapply(files, function(file) {
            filename <- basename(file)
            streamer <- FastqStreamer(file, n=n)
            chunk <- yield(streamer)
            chunkQuality <- quality(chunk)
            chunkBaseCall <- sread(chunk)
            chunkWidth <- width(chunk)
            enc <- encoding(chunkQuality)
            readAverageQuality <- alphabetScore(chunkQuality) / chunkWidth
            readWidth <- data.frame(table(chunkWidth))
            cycleQuality <- alphabetByCycle(chunkQuality)
            cycleBaseCall <- alphabetByCycle(chunkBaseCall)
            repeat {
                chunk <- yield(streamer)
                if (length(chunk) == 0) break
                chunkQuality <- quality(chunk)
                chunkBaseCall <- sread(chunk)
                chunkWidth <- width(chunk)
                readAverageQuality <- c(readAverageQuality, 
                                        alphabetScore(chunkQuality) / chunkWidth)
                readWidth <- rbind(readWidth, data.frame(table(chunkWidth)))
                readWidth <- ddply(readWidth, ~chunkWidth, summarise, Freq = sum(Freq))
                cycleQuality <- cycleQuality + alphabetByCycle(chunkQuality)
                cycleBaseCall <- cycleBaseCall + alphabetByCycle(chunkBaseCall)
            }
            close(streamer)
            .RqcResultSet(enc, filename, readAverageQuality, readWidth, 
                          cycleQuality, cycleBaseCall)
        })
    }
    names(rqcResultSet) <- basename(files)
    rqcResultSet
}
