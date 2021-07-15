app_ui <- function()
{

library(shiny)
library(shinybusy)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("G2PMineR"),


    fluidRow(
        column(4,
                h3("Upload Text"),
                fileInput("file1", "Choose CSV File",
                            multiple = TRUE,
                            accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")),
                actionButton("DoSubmitText","Submit File"),
                h3("Upload UniqueIDs"),
                fileInput("file2", "Choose CSV File",
                    multiple = TRUE,
                    accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
                actionButton("DoSubmitUniqueIDs","Submit File"),
                h3("Mining"),
                checkboxGroupInput("King","Choose a Kingdom",
                                    c("Plantae" = "P","Animalia" = "A","Fungi" = "F")),
                actionButton("DoMining","Perform Mining"),
                h3("Consensus Calculation"),
                actionButton("DoCons","Perform Consensus"),
                h3("Internal Network Analyses"),
                actionButton("DoINA","Perform Internal Network Analysis"),
                h3("Bipartite Analyses"),
                actionButton("DoBipartites","Perform Bipartite Analysis"),
                h3("Export All Outputs"),
                actionButton("ExportOut","Export")

            ),
            column(4,
                    h3("Progress Report"),
                    textOutput("result1"),
                    textOutput("result2"),
                    textOutput("result3"),
                    textOutput("result4"),
                    textOutput("result5"),
                    textOutput("result6"),
                    textOutput("result7"),
                    textOutput("result8"),
                    textOutput("result9")
                ),
            column(4,
                    h3("Plots"),
                    h4("Consensus Venn"),
                    plotOutput("PlotCV"),
                    h4("G, Ta, and P Bar Charts"),
                    plotOutput("PlotBarSpp"),
                    plotOutput("PlotBarGen"),
                    plotOutput("PlotBarPhen"),
                    h4("Internal Networks"),
                    plotOutput("PlotTaS"),
                    plotOutput("PlotTaG"),
                    plotOutput("PlotTaP"),
                    h4("Bipartite Plots"),
                    h5("PG"),
                    plotOutput("BipartitePG"),
                    h5("GS"),
                    plotOutput("BipartiteGS"),
                    h5("PS"),
                    plotOutput("BipartitePS")
                )
            )
    )

}
