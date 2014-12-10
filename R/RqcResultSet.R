setClass("RqcResultSet", contains=".QA")

.RqcResultSet <- function(enc, filename, readAverageQuality, readWidth, 
                          cycleQuality, cycleBaseCall)
{
    readWidth <- data.frame(width=readWidth$chunkWidth,
                            count=as.integer(readWidth$Freq),
                            filename=filename)
  
    readAverageQuality <- data.frame(average=readAverageQuality, 
                                     filename=filename)
  
    cycleQuality <- data.frame(t(cycleQuality[names(enc),]))
    names(cycleQuality) <- enc
    cycleQuality$cycle <- seq_len(nrow(cycleQuality))
    cycleQuality$filename <- filename
    
    cycleBaseCall <- cycleBaseCall[c("A", "C", "G", "T", "N"), ]
    cycleBaseCall <- data.frame(t(cycleBaseCall),
                                cycle=seq_len(ncol(cycleBaseCall)),
                                filename=filename)
  
    lst = list(perCycle=list(quality=cycleQuality,
                             baseCall=cycleBaseCall),
               perRead=list(width=readWidth,
                            averageQuality=readAverageQuality))
    new("RqcResultSet", .srlist=lst)
}
