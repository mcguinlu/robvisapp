library(shiny)
library(robvis)
library(svglite)
library(shinycssloaders)

options(spinner.color="#820000", spinner.type = 4, spinner.size = 2)

# Define UI for application that draws a histogram
ui <- tagList(
  tags$head(includeHTML("google-analytics.html")),
  tags$style(type="text/css", 
             ".shiny-output-error { visibility: hidden;}",
             ".shiny-output-error:before { visibility: visible; content: 'An error has occurred. Please double-check your that your selected tool matches the data you uploaded (common problem). If you believe you have found a bug, please contact me! The error is: ' ; }"
  ),
  shinyjs::useShinyjs(),  # Include shinyjs
  navbarPage(
    title = "robvis",
    theme = shinythemes::shinytheme("yeti"),
  
# Home Page ====================================================================          
  tabPanel(
    "Home",

      h1(strong(em("robvis"))),
      h3("Create publication quality risk-of-bias assessment figures"),
      
      hr(),

    
    
    sidebarLayout(
      sidebarPanel(
        h3(strong("About")),
        p(em("robvis"),"makes it easy to produce high quality figures that summarise the risk-of-bias assessments performed as part of a systematic review or research synthesis project."),
        br(),
        hr(),
        h3(strong("Citation")),
        p("If you use", em("robvis"), "to create risk-of-bias plots for your study, please remember to cite the tool."),
        p("More details and downloadable citation files can be found in the \"About\" tab."),
        
        
        br(),
        hr(),

        h3(strong("Found a bug?")),
        p("Please ", a(href="mailto:luke.mcguinness@bristol.ac.uk", "email me")),
        p(strong("OR")),
        p("Log an issue on", a(href="https://github.com/mcguinlu/robvis/issues", "GitHub")),
        br(),
        width = 3

      ),
      mainPanel(
        h3(strong("Using the tool")),
        h4(strong("Example datasets")),
        p("To help familiarise users with the tool, we have provided four example datasets which can be downloaded here:"),
        p(downloadButton("downloadROB2Data", "RoB2.0 dataset"),
          
          downloadButton("downloadROBINSData", "ROBINS dataset"),
          
          downloadButton("downloadQUADASData", "QUADAS dataset"),
          
          downloadButton("downloadROB1Data", "RoB1 dataset")),
        br(),        
        h4(strong("Setting up your own data")),
        p(strong("Required data format")),
        p("To ensure that this app works as expected, the uploaded risk-of-bias assessment summary table must follow a certain format.", 
          "For demonstration purposes, a correctly formatted summary table for the ROB2.0 tool is displayed below.", 
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
        ),
       p(strong("Example ROB2.0 summary table:")),

        tableOutput('rob2table')
        
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
                   "QUADAS-2" = "QUADAS-2",
                   "RoB 1/Generic" = "ROB1"
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
               
               numericInput("psize",
                            "Choose point size",
                            value = 15),
               
               selectInput(
                 "traffictextsize",
                 "Choose text size:",
                 c("8",
                   "10",
                   "12",
                   "14",
                   "16"
                 ),
                 selected = "12"
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
               
               downloadButton("downloadtrafficlightplot", "Download plot"),
               hr(),
               p("If you use", em("robvis"), "to produce figures for your publication, please remember to cite the tool. A citation can be found in the \"About\" tab.")
               

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
            "QUADAS-2" = "QUADAS-2",
            "RoB 1/Generic" = "ROB1"
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
        
        downloadButton("downloadsummaryplot", "Download plot"),
        hr(),
        p("If you use", em("robvis"), "to produce figures for your publication, please remember to cite the tool. A citation can be found in the \"About\" tab.")
        
        
      ),
      
      mainPanel(withSpinner(plotOutput("summaryplot")))
      
    )
  ),

# Weighted Bar Plot Page =======================================================          
tabPanel(
  "About",
  
  titlePanel(""),
  h3("Additional information"),
  h4("About the tool"),
  p(
    em("robvis"),
    "was developed by Luke McGuinness, drawing on work that took place during the",
    a("Evidence Synthesis Hackathon.", href = "https://www.eshackathon.org/"),
    "This web app is built on the",
    em("robvis"),
    "R package, which can be accessed ",
    a("here.", href = "https://www.github.com/mcguinlu/robvis"),
    em("robvis"),
    "forms part of the",
    a("metaverse", href = "https://www.github.com/rmetaverse"),
    ", a suite of tools for performing evidence synthesis in R."
  ),
  p("If you have questions about the tool or would like to provide feedback, please email ",
    a("luke.mcguinness@bristol.ac.uk", href = "mailto:luke.mcguinness@bristol.ac.uk"), 
    "."),
  br(),
  
  h4("About me"),
  p("Luke McGuinness is a National Insitute of Health Research Doctoral Research Fellow in Evidence Synthesis at Bristol Medical School, where he is examining the relationship between blood lipid levels and dementia risk.",
    "When procrastinating from real work, he is an R (pronounced \"oar\") and open science enthusiast."),
  p("Luke is part of the Bristol Appraisal and Review of Research (BARR) Group at the University of Bristol, led by Prof. Julian Higgins, which brings together researhers interested in the methodology",
    "and application of research synthesis methods such as systematic reviews, meta-analysis and critical assessment of research evidence."),
  br(),

  
  h3("Citing the tool"),
  p("If you use", em("robvis"), "to create risk-of-bias plots for your publication, please cite the tool using:"),
  tags$ul(
    tags$li("Luke A McGuinness (2019). robvis: An R package and web application for visualising risk-of-bias assessments. https://github.com/mcguinlu/robvis")),
  
  downloadButton("downloadbib", "Download .bib citation"),
  downloadButton("downloadris", "Download .ris citation"),
 
  br(),
  br(),
  
  h3("Acknowledgements"),
  p("This project would not have been possible without:"),
  tags$ul(
    tags$li("Prof. Julian Higgins, who as my main supervisor has been extremely supportive of this project;"),
    tags$li("Dr. Emily Kothe, who provided help on", em("ggplot"), "coding issues;"),
    tags$li("Eliza Grames, who create the amazing",em("robvis"), a("hex sticker;", href="https://github.com/mcguinlu/robvis/blob/master/man/figures/robvis_hex_box.png")),
    tags$li("The Baby Driver", a("soundtrack,", href="https://open.spotify.com/album/1XaJOcLe3xMQ611SMHtOja"), "which kept me sane while fixing coding bugs.")
       ),
  
  p("Additionally, the following people contributed valuable feedback that contributed to the development of this tool:",
    "Matthew Page, Alexandra Bannach-Brown, Kyle Hamilton, Charles Gray, Vincent Cheng, Wouter van Amsterdamn, Neal Haddaway and Martin Westgate."),
 
  br(),
  
  h3("Funding Statement"),
  p("Luke is funded by the National Institute for Health Research (NIHR) Doctoral Research Fellowship (DRF-2018-11-ST2-048).",
    "The views expressed are those of the author(s) and not necessarily those of the NIHR or the Department of Health and Social Care."),
  img(src="nihr_logo.jpg", align = "centre")
),
# FAQS =======================================================          
tabPanel(
  "FAQs",
  h1(strong("Coming soon. . . . "))
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
      write.csv(robvis::data_rob2, file, row.names = FALSE)
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
  
  output$downloadROB1Data <- downloadHandler(
    filename = function() {
      paste("ROB1_example", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(robvis::data_rob1, file, row.names = FALSE)
    }
  )
  
  output$downloadROB1Data <- downloadHandler(
    filename = function() {
      paste("ROB1_example", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(robvis::data_rob1, file, row.names = FALSE)
    }
  )
  
  output$downloadbib <- downloadHandler(
    filename = function() {
      paste("citation", ".bib", sep = "")
    },
    content = function(file) {
      file.copy("robviscitation.bib", file)
    }
  )
  
  output$downloadris <- downloadHandler(
    filename = function() {
      paste("citation", ".ris", sep = "")
    },
    content = function(file) {
      file.copy("robviscitation.ris", file)
    }
  )
  


  
  output$rob2table <- renderTable(head(robvis::data_rob2,4))

  
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
                        colour = input$barplotcolour)
          )
  })
  
output$summaryplot <- renderPlot({
  summaryplotInput()
  })


output$downloadsummaryplot <- downloadHandler(
    filename = function() {
      paste0(input$summarytool, ".", input$summarydownloadformat)
    },
    content = function(file) {
      shiny::withProgress(
        message = paste0("Downloading ", input$summarytool, " figure"),
        value = 0,
        {
          shiny::incProgress(7/10)
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
                        colour = input$trafficcolour, 
                        psize = input$psize) +
       ggplot2::theme(
         strip.text.x = ggplot2::element_text(size = input$traffictextsize),
         strip.text.y = ggplot2::element_text(angle = 180, size = input$traffictextsize),
         axis.title.x = ggplot2::element_text(size = input$traffictextsize),
         axis.title.y = ggplot2::element_text(size = input$traffictextsize)
         )
    
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
  nrows <- ifelse(nrows * (70/(20/input$psize)) + 100 < 750, 750, nrows * (70/(20/input$psize)))
  return(nrows)
  })

nrowsin <- reactive({
  req(input$trafficfile1)
  req(input$traffictool)
  trafficdf <- read.csv(input$trafficfile1$datapath,
                        header = TRUE)
  nrows <- nrow(trafficdf)
  nrows <- ifelse(nrows * 1/(20/input$psize) < 9, 9, nrows * 1/(20/input$psize))
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
      shiny::withProgress(
        message = paste0("Downloading ", input$traffictool, " figure"),
        value = 0,
        {
          shiny::incProgress(7/10)
          ggplot2::ggsave(
          file,
          plot = trafficlightplotInput(),
          device = input$trafficdownloadformat,
          width = (8-(2*(1/(20/input$psize)))),
          # width = (9 - (100/(80+input$psize))),
          height = nrowsin(),
          units = "in",
          dpi = 800, 
          limitsize = FALSE
        )
        }
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
