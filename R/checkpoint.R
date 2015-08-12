#' Save time storing longer analysis step on disk
#' 
#' This utility function can be used to save time on task that takes long time
#' to complete. A Rda file are written on disk containing only objects setted
#' to keep. If checkpoint function find related Rda file then this Rda will be
#' loaded.
#' 
#' 
#' @param label name of this code, will create a Rda file with the same name.
#' @param CODE R code.
#' @param path directory to write/load Rda file.
#' @param overwrite Rerun CODE and replace Rda file.
#' @param verbose argument passed to load function
#' @param keep vector of object/variable name to keep. NULL means error.
#' @return Nothing.
#' @note Experimental function.
#' @author Welliton Souza
#' @examples
#' 
#' checkpoint("Rqc", path=system.file(package="Rqc", "extdata"), {
#'   folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
#'   files <- list.files(full.names=TRUE, path=folder)
#'   rqcResultSet <- rqcQA(files, workers=1)
#' }, keep="rqcResultSet")
#' 
#' @export
checkpoint <- function(label, CODE, path = ".", overwrite = FALSE, verbose = FALSE, keep=NULL)
{
    rdaFile <- file.path(path, paste0(label, ".rda"))
    
    if (file.exists(rdaFile) & !overwrite) {
        load(rdaFile, parent.frame(1), verbose)
        message(paste(rdaFile, "has been loaded."))
    } else {
        CODE
        save(list = keep, file = rdaFile)
        message(paste(rdaFile, "has been written."))
    }
}
