.server.Rqc <- function(rqcResultSet) {
    plots <- list("Read Quality"=rqcReadQualityPlot, 
                  "Read Width"=rqcReadWidthPlot,
                  "Read Frequency"=rqcReadFrequencyPlot,
                  "Read Quality Box Plot"=rqcReadQualityBoxPlot,
                  "Cycle Average Quality PCA"=rqcCycleAverageQualityPcaPlot,
                  "Cycle Average Quality"=rqcCycleAverageQualityPlot,
                  "Cycle Quality"=rqcCycleQualityPlot,
                  "Cycle Quality Box Plot"=rqcCycleQualityBoxPlot)
    calcs <- list("Read Quality"=rqcReadQualityCalc, 
                  "Read Width"=rqcReadWidthCalc,
                  "Read Frequency"=rqcReadFrequencyCalc,
                  "Read Quality Box Plot"=rqcReadQualityBoxCalc,
                  "Cycle Average Quality PCA"=rqcCycleAverageQualityPcaCalc,
                  "Cycle Average Quality"=rqcCycleAverageQualityCalc,
                  "Cycle Quality"=rqcCycleQualityCalc,
                  "Cycle Quality Box Plot"=rqcCycleQualityBoxCalc)
    
    function(input, output, session) {
        output$rqcPlotExample <- renderPlot({
            validate(
                need(!is.null(input$group), "Must have at least one group selected."),
                need(!is.null(input$filename), "Must have at least one file selected.")
            )
            plots[[input$plot]](subsetByGroup(rqcResultSet[input$filename], input$group))
        })
        output$rqcTableExample <- renderTable({
            validate(
                need(!is.null(input$group), "Must have at least one group selected."),
                need(!is.null(input$filename), "Must have at least one file selected.")
            )
            calcs[[input$plot]](subsetByGroup(rqcResultSet[input$filename], input$group))
        })
    }
}

.ui.Rqc <- function(rqcResultSet)
{
    filenames <- names(rqcResultSet)
    groups <- unique(perFileInformation(rqcResultSet)$group)
    
    fluidPage(
        
        titlePanel(paste("Rqc", packageVersion("Rqc"), "- Interactive Quality Control Report")),
        
        sidebarLayout(
            sidebarPanel(
                selectInput("plot", "Available Graphics", list("Read Quality", "Read Width", "Read Frequency", "Read Quality Box Plot", "Cycle Average Quality PCA", "Cycle Average Quality", "Cycle Quality", "Cycle Quality Box Plot")),
                checkboxGroupInput("filename", "Select files to show on plots", filenames, selected = filenames),
                checkboxGroupInput("group", "Select groups to show on plots", groups, selected = groups)
            ),
            
            mainPanel(
                tabsetPanel(
                    tabPanel("Plot", plotOutput("rqcPlotExample")),
                    tabPanel("Data", tableOutput("rqcTableExample"))
                )
            )
        )
    )
}

#' Interactive Quality Control Report
#' 
#' This function runs a Shiny web application of interactive Rqc report. This is
#' useful for large amount of files and sample groups.
#' 
#' @param rqcResultSet list of \code{\link{RqcResultSet-class}} objects
#' @author Welliton Souza
#' @return function
#' @examples 
#' checkpoint("Rqc", path=system.file(package="Rqc", "extdata"), {
#'   folder <- system.file(package="ShortRead", "extdata/E-MTAB-1147")
#'   files <- list.files(full.names=TRUE, path=folder)
<<<<<<< HEAD
#'   rqcResultSet <- rqcQA(files, workers=1)
=======
#'   rqcResultSet <- rqcQA(files, pair=c(1,1), workers=1)
>>>>>>> master
#' }, keep="rqcResultSet")
#' # rqcShinyReport(rqcResultSet)
#' @export
rqcShinyReport <- function(rqcResultSet) {
    runApp(list(ui = .ui.Rqc(rqcResultSet), server = .server.Rqc(rqcResultSet)))
}
