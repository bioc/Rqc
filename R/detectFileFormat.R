#' Detect file format
#' @param files charactor vector of file names
#' @return list of FastqFile and BamFiles objects
#' @examples
#' folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
#' files <- list.files(full.names=TRUE, path=folder)
#' input <- detectFileFormat(files)
#' sapply(input, class)
#' @export
detectFileFormat <- function(files)
{
    f <- function(file) {
        if (grepl("fastq", file, ignore.case = FALSE))
            FastqFile(file)
        else if (grepl("bam", file, ignore.case = FALSE))
            BamFile(file)
        else
            stop(paste("File format not detected or supported:", basename(file)))
    }
    lapply(files, f)
}