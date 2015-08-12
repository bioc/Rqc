library(Rqc)

context("Reduced Representation of DNA sequences")

asc <- function(x) { strtoi(charToRaw(x),16L) }

test_that("convert DNA to RRDNA", {
    expect_equal(asc(toRRDNA("A")), c(2,2))
    expect_equal(asc(toRRDNA("T")), c(2,3))
    expect_equal(asc(toRRDNA("C")), c(2,4))
    expect_equal(asc(toRRDNA("G")), c(2,5))
    expect_equal(asc(toRRDNA("N")), c(2,1))
})

test_that("sequence divisible by 3 at the first character", {
    expect_equal(asc(toRRDNA("AAA"))[1], 1)
    expect_equal(asc(toRRDNA("AA"))[1], 3)
    expect_equal(asc(toRRDNA("A"))[1], 2)
})

test_that("revert RRDNA to DNA", {
    expect_equal(fromRRDNA(toRRDNA("A")), "A")
    expect_equal(fromRRDNA(toRRDNA("T")), "T")
    expect_equal(fromRRDNA(toRRDNA("C")), "C")
    expect_equal(fromRRDNA(toRRDNA("G")), "G")
    expect_equal(fromRRDNA(toRRDNA("N")), "N")
    expect_equal(fromRRDNA(toRRDNA("ATC")), "ATC")
    expect_equal(fromRRDNA(toRRDNA("AT")), "AT")
})

test_that("dist matrix", {
    expect_equal(matdist(toRRDNA("AAA")), matrix(0))
    expect_equal(matdist(toRRDNA(c("AAA", "AAA"))), matrix(c(0,0,0,0), ncol = 2))
    expect_equal(matdist(toRRDNA(c("AAA", "CCC"))), matrix(c(0,3,3,0), ncol = 2))
    expect_equal(matdist(toRRDNA(c("A", "AAA"))), matrix(c(0,2,2,0), ncol = 2))
})

fastqFile <- system.file(package = "ShortRead", "extdata/E-MTAB-1147/ERR127302_1_subset.fastq.gz")
DNA <- as.character(sread(readFastq(fastqFile)))
test_that("RRDNA real data", {
    DNA.rr <- toRRDNA(DNA)
    DNA.reverted <- fromRRDNA(DNA.rr)
    expect_equal(DNA, DNA.reverted)
})
