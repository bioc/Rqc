#' Detect file format
#' @param file file name
#' @return FastqFile or BamFiles objects
#' @examples
#' folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
#' files <- list.files(full.names=TRUE, path=folder)
#' input <- lapply(files, detectFileFormat)
#' sapply(input, class)
#' @export
detectFileFormat <- function(file)
{
    if (grepl("fastq|fq", file, ignore.case = FALSE))
        FastqFile(file)
    else if (grepl("bam", file, ignore.case = FALSE))
        BamFile(file)
    else
        stop(paste("File format not detected or supported:", basename(file)))
}