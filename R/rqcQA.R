rqcQA <- function(files, sample = TRUE, n = 1e6, 
                  groupFactor = rep("None", length(files)),
                  workers = multicoreWorkers())
{
  if(length(files) == 0) stop("Input files were not provided.")
  filesNotFound <- files[!file.exists(files)]
  if (length(filesNotFound) != 0) {
    for (f in filesNotFound) warning(paste(f, "not found."))
    stop("One or more input files were not found.")
  }
  if(length(groupFactor) != length(files)) 
      stop("groupFactor argument must have the same length of files argument.")
  
  inputs <- lapply(seq_along(files), function(i) {
      list(file=files[i], groupFactor=groupFactor[i])
  })

  if (workers > 1) {
      param <- MulticoreParam(workers, tasks=length(files), stop.on.error=TRUE)
  } else {
      param <- SerialParam()
  }
 
  rqcResultSet <- bplapply(inputs, function(input) {
    file <- input$file
    con <- if (sample) FastqSampler(file, n) else FastqStreamer(file, n)
    chunk <- yield(con)

    readQuality <- .readQuality(chunk)
    readWidth <- .readWidth(chunk)
    cycleQuality <- .cycleQuality(chunk)
    cycleBasecall <- .cycleBasecall(chunk)

    chunk <- yield(con)
    while (!sample & length(chunk) != 0) {
      readQuality <- .mergeReadQuality(readQuality, .readQuality(chunk))
      readWidth <- .mergeReadWidth(readWidth, .readWidth(chunk))
      cycleQuality <- .mergeCycleQuality(cycleQuality, .cycleQuality(chunk))
      cycleBasecall <- .mergeCycleBasecall(cycleBasecall, .cycleBasecall(chunk))

      chunk <- yield(con)
    }
    close(con)

    fileInfo <- .fileInfo(input)

    lst = list(perFile=list(information=fileInfo),
               perCycle=list(quality=cycleQuality, baseCall=cycleBasecall),
               perRead=list(width=readWidth, averageQuality=readQuality))
    new("RqcResultSet", .srlist=lst)
  }, BPPARAM=param)
  names(rqcResultSet) <- basename(files)
  return(rqcResultSet)
}
