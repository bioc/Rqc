library(Rqc)

context("internal merge functions")

fastqFile <- system.file(package = "ShortRead", "extdata/E-MTAB-1147/ERR127302_1_subset.fastq.gz")
fq <- readFastq(fastqFile)
streamer <- FastqStreamer(fastqFile, n = 10000)
chunk1 <- yield(streamer)
chunk2 <- yield(streamer)
close(streamer)

test_that("mergeReadWidth",{
    expected <- .readWidth(fq)
    actual <- .mergeReadWidth(.readWidth(chunk1), .readWidth(chunk2))
    expect_equal(expected, actual)
})

test_that("mergeReadQuality",{
    expected <- .readQuality(fq)
    actual <- .mergeReadQuality(.readQuality(chunk1), .readQuality(chunk2))
    expect_equal(expected, actual)
})

test_that("mergeCycleQuality",{
    expected <- .cycleQuality(fq)
    actual <- .mergeCycleQuality(.cycleQuality(chunk1), .cycleQuality(chunk2))
    expect_equal(expected, actual)
})

test_that("mergeCycleBasecall",{
    expected <- .cycleBasecall(fq)
    actual <- .mergeCycleBasecall(.cycleBasecall(chunk1), .cycleBasecall(chunk2))
    expect_equal(expected, actual)
})

test_that("mergeReadFrequency",{
    expected <- .readFrequency(fq)
    actual <- .mergeReadFrequency(.readFrequency(chunk1), .readFrequency(chunk2))
    expect_equal(expected, actual)
})