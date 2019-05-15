library(shiny)
library(robvis)
library(svglite)
library(shinycssloaders)

options(spinner.color="#820000", spinner.type = 4, spinner.size = 2)

# Define UI for application that draws a histogram
ui <- tagList(  
  tags$style(type="text/css", 
             ".shiny-output-error { visibility: hidden;}",
             ".shiny-output-error:before { visibility: visible; content: 'An error has occurred. Please double-check your that your selected tool matches the data you uploaded (common problem). If you believe you have found a bug, please contact me! The error is: ' ; }"
  ),
  shinyjs::useShinyjs(),  # Include shinyjs
  navbarPage( "robvis",
  
# Home Page ====================================================================          
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

# Traffic light Plot Page ======================================================    
  tabPanel("Traffic Light Plot",
           titlePanel("Traffic light visualisation of risk-of-bias assessments"),
           
           sidebarLayout(
             sidebarPanel(
               h4(tags$b("Set-up")),
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
                 "Specify assessment tool used:",
                 c(
                   Choose = '',
                   "RoB 2.0" = "ROB2",
                   "ROBINS-I" = "ROBINS-I",
                   "QUADAS-2" = "QUADAS-2"
                 )
               ),
               actionButton("resettraffic", "Reset"),
               hr(),
               h4(tags$b("Options")),
               
               selectInput(
                 "trafficcolour",
                 "Choose colour scheme:",
                 c(
                   "Cochrane colours" = "cochrane",
                   "Colourblind-friendly" = "colourblind"
                 )
               ),
               
               hr(),
               h4(tags$b("Download")),
               selectInput(
                 "trafficdownloadformat",
                 "Specify download format:",
                 c(
                   Choose = '',
                   ".png" = "png",
                   ".jpeg" = "jpeg",
                   ".tiff" = "tiff",
                   ".eps" = "eps"
                 )
               ),
               
               downloadButton("downloadtrafficlightplot", "Download plot")
               

             ),
             
             mainPanel(
               withSpinner(uiOutput("trafficplotUI"))
               )
           )
  ),

# Weighted Bar Plot Page =======================================================          
  tabPanel(
    "Weighted Summary Plot",
    
    titlePanel("Weighted barplot visualisation of risk-of-bias assessments"),
    
    sidebarLayout(
      sidebarPanel(
        h4(tags$b("Set-up")),
        fileInput(
          "summaryfile1",
          "Choose CSV file:",
          multiple = FALSE,
          accept = c("text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv")
        ),
        
        selectInput(
          "summarytool",
          "Specify assessment tool used:",
          c(
            Choose = '',
            "RoB 2.0" = "ROB2",
            "ROBINS-I" = "ROBINS-I",
            "QUADAS-2" = "QUADAS-2"
          )
        ),
        actionButton("resetbarplot", "Reset"),
        hr(),
        h4(tags$b("Options")),
        
        checkboxInput("weights",
                      "Use weights (strongly recommended)?", 
                      value = TRUE),  
        
        checkboxInput("overall",
                      "Include overall risk of bias?"),    
        
        selectInput(
          "barplotcolour",
          "Choose colour scheme:",
          c(
            "Cochrane colours" = "cochrane",
            "Colourblind-friendly" = "colourblind"
          )
        ),
        
        
        hr(),
        h4(tags$b("Download")),
        selectInput(
          "summarydownloadformat",
          "Specify download format:",
          c(
            Choose = '',
            ".png" = "png",
            ".jpeg" = "jpeg",
            ".tiff" = "tiff",
            ".eps" = "eps"
            )
        ),
        
        downloadButton("downloadsummaryplot", "Download plot")
        
        
      ),
      
      mainPanel(withSpinner(plotOutput("summaryplot")))
    )
  )
           )
)



# Server =======================================================================
server <- function(input, output) {

  
  observeEvent(input$cochrane, {
    # Change the following line for more examples
    shinyjs::toggleState("usercolours")
  })

  #Download datasets
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
    req(input$summaryfile1)
    req(input$summarytool)
    
    tryCatch({
      df <- read.csv(input$summaryfile1$datapath,
                     header = TRUE)
    },
    error = function(e) {
      stop(safeError(e))
    })
    
    try(robvis::rob_summary(data = df,
                        tool = input$summarytool,
                        overall = input$overall, 
                        weighted = input$weights,
                        colour = input$barplotcolour))
  })
  
output$summaryplot <- renderPlot({
  summaryplotInput()
  })


output$downloadsummaryplot <- downloadHandler(
    filename = function() {
      paste0(input$summarytool, ".", input$summarydownloadformat)
    },
    content = function(file) {
      ggplot2::ggsave(
        file,
        plot = summaryplotInput(),
        device = input$summarydownloadformat,
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
      stop(safeError(paste('Cannot read data file')))
    })

    
      robvis::rob_traffic_light(data = trafficdf,
                        tool = input$traffictool,
                        colour = input$trafficcolour)
    
  })
  
output$trafficlightplot <- renderPlot({
    trafficlightplotInput()
})

nrowspx <- reactive({
  req(input$trafficfile1)
  req(input$traffictool)
  trafficdf <- read.csv(input$trafficfile1$datapath,
                        header = TRUE)
  nrows <- nrow(trafficdf)
  nrows <- nrows * 70 + 100
  return(nrows)
  })

nrowsin <- reactive({
  req(input$trafficfile1)
  req(input$traffictool)
  trafficdf <- read.csv(input$trafficfile1$datapath,
                        header = TRUE)
  nrows <- nrow(trafficdf)
  nrows <- nrows * 1
  return(nrows)
})

output$trafficplotUI <- renderUI({
  withSpinner(plotOutput("trafficlightplot", height = nrowspx()))
})



output$downloadtrafficlightplot <- downloadHandler(
    filename = function() {
      paste0(input$traffictool,".", input$trafficdownloadformat)
    },
    content = function(file) {
      ggplot2::ggsave(
        file,
        plot = trafficlightplotInput(),
        device = input$trafficdownloadformat,
        width = 8,
        height = nrowsin(),
        units = "in",
        dpi = 800, 
        limitsize = FALSE
      )
    }
  )

observeEvent(input$resettraffic, {
  shinyjs::reset("trafficfile1")
  shinyjs::reset("traffictool")})

observeEvent(input$resetbarplot, {
  shinyjs::reset("summaryfile1")
  shinyjs::reset("summarytool")})
}

# Run the application
shinyApp(ui = ui, server = server)
