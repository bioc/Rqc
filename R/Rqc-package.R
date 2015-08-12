#' Quality Control Tool for High-Throughput Sequencing Data
#' 
#' Rqc is an optimized tool designed for quality assessment of high-throughput
#' sequencing data. It performs parallel processing of entire files and
#' produces a report, which contains a set of high-resolution images that can
#' be directly used on publications.
#' 
#' 
#' @name Rqc-package
#' @aliases Rqc Rqc-package
#' @docType package
#' @author Welliton Souza, Benilton Carvalho
#' 
#' Maintainer: Welliton Souza <well309@@gmail.com>
#' @keywords package
#' @examples
#' 
#'   options(device.ask.default = FALSE)
#'   folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
#'   rqc(folder, ".fastq.gz", workers=1)
#'   
#' @useDynLib Rqc 
#' @import methods
#' @importClassesFrom ShortRead .QA
#' @importMethodsFrom BiocGenerics do.call lapply ncol nrow paste rbind setdiff table unique width
#' @importMethodsFrom BiocParallel bplapply bpmapply
#' @importMethodsFrom Biostrings encoding quality
#' @importMethodsFrom IRanges mean quantile sub
#' @importMethodsFrom S4Vectors Rle mcols
#' @importMethodsFrom ShortRead alphabetByCycle alphabetScore FastqStreamer sread yield ShortReadQ
#' @importMethodsFrom Rsamtools path
#' @importMethodsFrom GenomicAlignments readGAlignments
#' @importFrom ggplot2 aes aes_string geom_bar geom_boxplot geom_line geom_point ggplot labs scale_fill_gradient scale_colour_manual scale_fill_manual scale_x_discrete qplot facet_wrap scale_y_discrete geom_tile geom_histogram coord_flip theme_bw geom_hline geom_vline geom_text geom_segment scale_x_log10
#' @importFrom knitr knit2html opts_knit kable
#' @importFrom ShortRead FastqSampler FastqFile 
#' @importFrom BiocStyle markdown 
#' @importFrom plyr ddply summarize 
#' @importFrom knitr knit 
#' @importFrom markdown markdownToHTML 
#' @importFrom BiocParallel bpparam MulticoreParam SerialParam multicoreWorkers 
#' @importFrom grid arrow unit 
#' @importFrom reshape2 dcast 
#' @importFrom digest digest 
#' @importFrom Rcpp evalCpp sourceCpp
#' @importFrom biovizBase colorBlindSafePal  getBioColor
#' @importFrom Rsamtools BamFile ScanBamParam yieldSize<- scanBam
#' @importFrom shiny renderPlot validate need renderTable fluidPage titlePanel sidebarLayout sidebarPanel selectInput checkboxGroupInput mainPanel tabsetPanel tabPanel plotOutput tableOutput runApp 
#' @importFrom GenomicFiles reduceByYield REDUCEsampler
NULL
