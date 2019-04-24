library(shiny)
library(robvis)

# Define UI for application that draws a histogram
ui <- navbarPage("robvis",
  tabPanel("Data Format",
           
           titlePanel("Data format needed for this app to work"),
           
           sidebarLayout(
             sidebarPanel(
               h4("Introduction to robvis"),
               
               br(),
               h4("Data format needed for this app to work"),
               p("To ensure that this app works as expected, the required data format is 
                 displayed on the right. Data are laid out as follows:"),
               tags$ul(
                 tags$li("The first column contains details about the study, such as author and year of publication."),
                 tags$li("The next column contains the first domain of the assessment tool. The number of columns that contain a risk of bias domain will vary by tool used."),
                 tags$li("The second to last column contains the overall risk-of-bias judgement"),
                 tags$li("The final column contains the weight variable, often study sample size or precision.")
               ),
               h4("robvis R package"),
               p("This web app is built on the robvis R package, which can be accessed at:")
             ),
             mainPanel(
             h4("Example of assessment sheet using ROB2"),
             tableOutput('rob2table'),
             br(),
             h4("Example of assessment sheet using ROBINS-I"),
             tableOutput('robinstable'),
             br(),
             h4("Example of assessment sheet using QUADAS-2"),
             tableOutput('quadastable')))
           ),

  tabPanel(
    "Summary",
    
    titlePanel("Risk-of-bias summary visualisation"),
    
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
          "Tool:",
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
  tabPanel("Traffic Light")
)



server <- function(input, output) {
  
  output$rob2table <- renderTable(
    robvis::data_rob)
  output$robinstable <- renderTable(
    robvis::data_robins)
  output$quadastable <- renderTable(
    robvis::data_quadas)
  
  
  
  summaryplotInput <- reactive({
    req(input$file1)
    
    tryCatch({
      df <- read.csv(input$file1$datapath,
                     header = TRUE)
    },
    error = function(e) {
      stop(safeError(e))
    })
    
    robvis::rob_summary(data = df,
                        tool = input$tool,
                        overall = input$overall)
    
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
}

# Run the application
shinyApp(ui = ui, server = server)
