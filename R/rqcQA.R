rqcQA <- function(files, sample = TRUE, n = 1e6, BPPARAM=bpparam())
{
  if(length(files) == 0) stop("Input files were not provided.")
  filesNotFound <- files[!file.exists(files)]
  if (length(filesNotFound) != 0) {
    for (f in filesNotFound) warning(paste(f, "not found."))
    stop("One or more input files were not found.")
  }

  rqcResultSet <- bplapply(files, function(file) {
    con <- if (sample) FastqSampler(file, n) else FastqStreamer(file, n)
    chunk <- yield(con)

    readQuality <- .readQuality(chunk)
    readWidth <- .readWidth(chunk)
    tileQuality <- .tileQuality(chunk)
    cycleQuality <- .cycleQuality(chunk)
    cycleBasecall <- .cycleBasecall(chunk)

    chunk <- yield(con)
    while (!sample & length(chunk) != 0) {
      readQuality <- .mergeReadQuality(readQuality, .readQuality(chunk))
      readWidth <- .mergeReadWidth(readWidth, .readWidth(chunk))
      tileQuality <- .mergeTileQuality(tileQuality, .tileQuality(chunk))
      cycleQuality <- .mergeCycleQuality(cycleQuality, .cycleQuality(chunk))
      cycleBasecall <- .mergeCycleBasecall(cycleBasecall, .cycleBasecall(chunk))

      chunk <- yield(con)
    }
    close(con)

    fileInfo <- .fileInfo(file)

    lst = list(perFile=list(information=fileInfo),
               perCycle=list(quality=cycleQuality, baseCall=cycleBasecall),
               perRead=list(width=readWidth, averageQuality=readQuality),
               perTile=list(meanQuality=tileQuality))
    new("RqcResultSet", .srlist=lst)
  }, BPPARAM=BPPARAM)
  names(rqcResultSet) <- basename(files)
  return(rqcResultSet)
}
