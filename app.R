library(shiny)
library(robvis)

# Define UI for application that draws a histogram
ui <- navbarPage(
  "robvis",
  tabPanel(
    "Home",
    
    titlePanel("Welcome!"),
    
    sidebarLayout(
      sidebarPanel(
        h4(
          "Introducing", em("robvis,"),"a tool to visualise risk of bias assessments"
        ),
        p(
          "This app makes it easy to produce publication quality figures that summarise the risk-of-bias assessments performed as part of a systemtatic review."
        ),
        p(
          "This web app is built on the",
          em("robvis"),
          "R package, which can be accessed ",
          a("here.", href = "https://www.github.com/mcguinlu/robvis")
        ),
        p(
          em("robvis"),
          "forms part of the",
          a("metaverse", href = "https://www.github.com/rmetaverse"),
          ", a suite of tools for performing evidence synthesis in R."
        ),
        br(),
        h4("Required data format"),
        p(
          
          "To ensure that this app works as expected, the uploaded table must have a certain format.", 
          "For demonstration purposes, an correctly formatted summary table for each tool is displayed on the right.", 
          "For clarity, data are laid out as follows:"
        ),
        tags$ul(
          tags$li(
            "The first column contains details about the study, such as author and year of publication."
          ),
          tags$li(
            "The second and subsequent columns contain the judgements in each domain of the assessment tool. The number of columns containing domain-level assessments will vary by tool used.",
            strong("NB: it is important that the order of domains in the uploaded spreadsheet is exactly the same as the order in which the domains appear in the tool used.")
          ),
          tags$li(
            "The second last column contains the overall risk-of-bias judgement"
          ),
          tags$li(
            "The final column contains the \"Weight\" variable, often study sample size or precision."
          )
        )

      ),
      mainPanel(
        h4("Example of summary assessment sheet using ROB2"),
        tableOutput('rob2table'),
        br(),
        h4("Example datasets for use with this app can be downloaded here:"),
        downloadButton("downloadROB2Data", "Download RoB2.0 example dataset"),
        br(),
        br(),
        downloadButton("downloadROBINSData", "Download ROBINS example dataset"),
        br(),
        br(),
        downloadButton("downloadQUADASData", "Download QUADAS example dataset")
        # br(),
        # h4("Example of assessment sheet using ROBINS-I"),
        # tableOutput('robinstable'),
        # br(),
        # h4("Example of assessment sheet using QUADAS-2"),
        # tableOutput('quadastable')
      )
    )
  ),
  
  tabPanel(
    "Weighted Summary Plot",
    
    titlePanel("Weighted risk-of-bias summary visualisation"),
    
    sidebarLayout(
      sidebarPanel(
        fileInput(
          "file1",
          "Choose CSV file:",
          multiple = FALSE,
          accept = c("text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv")
        ),
        
        selectInput(
          "tool",
          "Assessment tool used:",
          c(
            Choose = '',
            "RoB 2.0" = "ROB2",
            "ROBINS-I" = "ROBINS-I",
            "QUADAS-2" = "QUADAS-2"
          )
        ),
        
        checkboxInput("overall", "Include overall risk of bias?", value = FALSE),
        
        downloadButton("downloadsummaryplot", "Download plot (.png)")
        
        
      ),
      
      mainPanel(plotOutput("summaryplot"))
    )
  ),
  
  tabPanel("Traffic Light Plot",
           titlePanel("Traffic light risk-of-bias summary visualisation"),
           
           sidebarLayout(
             sidebarPanel(
               fileInput(
                 "trafficfile1",
                 "Choose CSV file:",
                 multiple = FALSE,
                 accept = c("text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")
               ),
               
               selectInput(
                 "traffictool",
                 "Assessment tool used:",
                 c(
                   Choose = '',
                   "RoB 2.0" = "ROB2",
                   "ROBINS-I" = "ROBINS-I",
                   "QUADAS-2" = "QUADAS-2"
                 )
               ),
               
               downloadButton("downloadtrafficlightplot", "Download plot (.png)")
             ),
             
             mainPanel(uiOutput("trafficplotUI"))
           )
  )     
           )




server <- function(input, output) {
  # library(Cairo)
  # options(shiny.usecairo=TRUE)

  output$downloadROB2Data <- downloadHandler(
    filename = function() {
      paste("ROB2_example", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(robvis::data_rob, file, row.names = FALSE)
    }
  )
  
  output$downloadROBINSData <- downloadHandler(
    filename = function() {
      paste("ROBINS_example", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(robvis::data_robins, file, row.names = FALSE)
    }
  )
  
  output$downloadQUADASData <- downloadHandler(
    filename = function() {
      paste("QUADAS_example", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(robvis::data_quadas, file, row.names = FALSE)
    }
  )
  
  output$rob2table <- renderTable(robvis::data_rob)
  output$robinstable <- renderTable(robvis::data_robins)
  output$quadastable <- renderTable(robvis::data_quadas)
  
  
# Summary plot and download  
summaryplotInput <- reactive({
    req(input$file1)
    req(input$tool)
    
    tryCatch({
      df <- read.csv(input$file1$datapath,
                     header = TRUE)
    },
    error = function(e) {
      stop(safeError(e))
    })
    
    try(robvis::rob_summary(data = df,
                        tool = input$tool,
                        overall = input$overall))
    
  })
  
output$summaryplot <- renderPlot({
    summaryplotInput()
  })
  

output$downloadsummaryplot <- downloadHandler(
    filename = function() {
      paste0(input$tool, ".png")
    },
    content = function(file) {
      ggplot2::ggsave(
        file,
        plot = summaryplotInput(),
        device = "png",
        width = 8,
        height = 2.41,
        dpi = 800
      )
    }
  )
  
# Traffic light plot and download
trafficlightplotInput <- reactive({
    req(input$trafficfile1)
    req(input$traffictool)
    
    tryCatch({
      trafficdf <- read.csv(input$trafficfile1$datapath,
                     header = TRUE)
    },
    error = function(e) {
      stop(safeError(e))
    })
    
    try(robvis::rob_traffic_light(data = trafficdf,
                        tool = input$traffictool))
    
  })
  
output$trafficlightplot <- renderPlot({
  trafficlightplotInput()
})

nrows <- reactive({
  req(input$trafficfile1)
  req(input$traffictool)
  trafficdf <- read.csv(input$trafficfile1$datapath,
                        header = TRUE)
  nrows <- nrow(trafficdf)
  nrows <- nrows * 70 + 100
  return(nrows)
  })

output$trafficplotUI <- renderUI({
  plotOutput("trafficlightplot", height = nrows())
})



output$downloadtrafficlightplot <- downloadHandler(
    filename = function() {
      paste0(input$traffictool, ".png")
    },
    content = function(file) {
      ggplot2::ggsave(
        file,
        plot = trafficlightplotInput(),
        device = "png",
        width = 8,
        height = 8,
        dpi = 800
      )
    }
  )

}

# Run the application
shinyApp(ui = ui, server = server)
