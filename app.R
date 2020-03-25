###########################################################################
# About -------------------------------------------------------------------
###########################################################################

# Title: Risk of Bias Visualiation web application
# Author: Luke A McGuinness (luke.mcguinness@bristol.ac.uk)
# Description: This app was created to be a companion to the robvis R package
# (https://github.com/mcguinlu/medrxivr), which allows users to create
# publication quality risk-of-bias plots. 
#
# Other notes: Submitted to the Shiny Contest 2020


###########################################################################
# External scripts --------------------------------------------------------
###########################################################################

# Library calls
source("R/library.R")

###########################################################################
# User interface ----------------------------------------------------------
###########################################################################
ui <- tagList(
  
  # Add Google analytics and error handling CSS
  tags$head(includeHTML("google-analytics.html")),
  tags$style(
    type = "text/css",
    "#image img {max-width: 80%; width: 100%; height: auto}",
    ".shiny-output-error { visibility: hidden;}",
    ".shiny-output-error:before { visibility: visible; content: 'An error has occurred. Please double-check your that your selected tool matches the data you uploaded (common problem). If you believe you have found a bug, please contact me! The error is: ' ; }"
  ),
  
  # Load dependencies
  shinyjs::useShinyjs(),
  use_waiter(),
  use_waitress(color = "#333333"),
  useShinyalert(),
  
  # Define navbar page
  navbarPage(
    id = "mytabsetpanel",
    title = "robvis",
    theme = shinythemes::shinytheme("yeti"),
    
    # Home Page ====================================================================
    tabPanel(
      "Home",
      
      h1(strong(em("robvis"))),
      h3("Create publication quality risk-of-bias assessment figures"),
      hr(),
      fluidRow(
        column(
          8,
          title = "About",
          id = "home_about",
          sidebarLayout(
            sidebarPanel(
              h3(strong("About")),
              p(
                em("robvis"),
                "makes it easy to produce high quality figures that summarise the risk-of-bias assessments performed as part of a systematic review or research synthesis project."
              ),
              br(),
              hr(),
              h3(strong("Citation")),
              p(
                "If you use",
                em("robvis"),
                "to create risk-of-bias plots for your study, please remember to cite the tool."
              ),
              p(
                "More details and downloadable citation files can be found in the ",
                actionLink("gotoabout", "\"About\""),
                "tab."
              ),
              
              
              br(),
              hr(),
              
              h3(strong("Found a bug?")),
              p(
                "Please ",
                a(href = "mailto:luke.mcguinness@bristol.ac.uk?cc=risk-of-bias@bristol.ac.uk&body=Hi,%20%0d%0dThis%20is%20a%20template%20to%20help%20me%20fix%20your%20issue%20as%20quickly%20as%20possible.%20Please%20make%20sure%20you:%0d-%20detail%20your%20issue,%20and%20where%20it%20occured%20[e.g.%20data%20upload,%20generating%20graphs]%0d-%20attach%20the%20file%20you%20were%20trying%20to%20upload%0d%0dThanks,%0d%0dLuke&Subject=robvis%20Query", "email me")
              ),
              p(strong("OR")),
              p(
                "Log an issue on",
                a(href = "https://github.com/mcguinlu/robvis/issues", "GitHub")
              ),
              br(),
              width = 4
              
            ),
            mainPanel(
              fluidRow(
                column(width = 6, h3("Quick start")),
                column(
                  width = 6,
                  align = "right",
                  br(),
                  actionButton("gotodata", "Upload your data")
                )
              ),
              
              includeMarkdown("text/using.md"),
              br(),
              div(
                align = "center",
                
                p(
                  downloadButton("downloadROB2Data", "RoB2.0 dataset"),
                  
                  downloadButton("downloadROBINSData", "ROBINS dataset"),
                  
                  downloadButton("downloadQUADASData", "QUADAS dataset"),
                  
                  downloadButton("downloadROB1Data", "Generic dataset")
                )
              ),
              includeMarkdown("text/funding.md")
              
              
            )
          )
        ),
        column(
          4,
          align = "center",
          title = "Example",
          id = "home_example",
          br(),
          imageOutput("image")
        )
      ),
      waiter_show_on_load(html = tagList(spin_4(),
                                         br(),
                                         h4("Loading robvis")),
                          color = "#333333")
    ),
    
    # Set-up ------------------------------------------------------------------
    
    tabPanel(
      "Upload data",
      value = "data",
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(width = 6, h4(tags$b(
              "Select assessment tool"
            ))),
            column(
              width = 6,
              align = "right",
              actionButton(
                "tool_help",
                label = "Help",
                icon = icon("question-circle")
              )
            )
          ),
          selectInput(
            "tool",
            "Specify the assessment tool used:",
            c(
              Choose = '',
              "RoB 2.0" = "ROB2",
              "ROBINS-I" = "ROBINS-I",
              "QUADAS-2" = "QUADAS-2",
              "Generic" = "Generic"
              # "ROBINS-I Online" = "ROBINS-I Online"
            )
          ),
          
          
          hidden(div(
            id = "test",
            hr(),
            fluidRow(
              column(width = 6, h4(tags$b("Load data"))),
              column(
                width = 6,
                align = "right",
                actionButton(
                  "data_help",
                  label = "Help",
                  icon = icon("question-circle")
                )
              )
            ),
            fileInput("data",
                      "Choose data file:",
                      multiple = FALSE,),
            p(
              "Alternatively, you can ",
              actionLink("enter_manually", "enter your data manually."),
              hr(),
              span(p(textOutput("warnings_header")), style = "color:red"),
              span(p(textOutput(
                "duplicate_studies"
              )), style = "color:red"),
              span(p(textOutput("wrong_ncol")), style = "color:red"),
              span(p(textOutput("wrong_levels")), style = "color:red"),
              
              br(),
              actionButton("gen_plots", "Generate Plots"),
              actionButton("reset", "Reset"),
              br(),
            )
          ))
        ),
        mainPanel(fluidRow(
          column(
            width = 11,
            h3("Review your data"),
            textOutput("data_text"),
            DT::dataTableOutput("mytable"),
            br(),
            actionButton("new_row", "Add a new study")
          )
        ))
      )
    ),
    
    # Traffic light Plot Page ======================================================
    tabPanel("Plots",
             value = "plots",
             tabsetPanel(
               tabPanel(
                 "Traffic Light Plot",
                 value = "tf-panel",
                 sidebarLayout(
                   sidebarPanel(
                     h3("Traffic-light plot"),
                     hr(),
                     
                     fluidRow(
                       column(width = 6, h4(tags$b("Options"))),
                       column(
                         width = 6,
                         align = "right",
                         actionButton(
                           "tf_help",
                           label = "Help",
                           icon = icon("question-circle")
                         )
                       )
                     ),
                     
                     
                     selectInput(
                       "trafficcolour",
                       "Choose colour scheme:",
                       c(
                         "Cochrane colours" = "cochrane",
                         "Colourblind-friendly" = "colourblind"
                       )
                     ),
                     
                     numericInput("psize",
                                  "Choose point size:",
                                  value = 15),
                     
                     selectInput(
                       "traffictextsize",
                       "Choose text size:",
                       c("8",
                         "10",
                         "12",
                         "14",
                         "16"),
                       selected = "12"
                     ),
                     
                     hr(),
                     h4(tags$b("Download")),
                     selectInput(
                       "trafficdownloadformat",
                       "Specify download format:",
                       c(
                         ".png" = "png",
                         ".jpeg" = "jpeg",
                         ".tiff" = "tiff",
                         ".eps" = "eps"
                       )
                     ),
                     
                     downloadButton("downloadtrafficlightplot", "Download plot"),
                     hr(),
                     p(
                       "If you use",
                       em("robvis"),
                       "to produce figures for your publication, please remember to cite the tool. A citation can be found in the \"About\" tab."
                     )
                     
                     
                   ),
                   
                   mainPanel(br(),
                             br(),
                             uiOutput("trafficplotUI"))
                 )
               ),
               
               # Weighted Bar Plot Page =======================================================
               tabPanel(
                 "Weighted Summary Plot",
                 value = "sum-tab",
                 
                 sidebarLayout(
                   sidebarPanel(
                     h3("Weighted Summary Plot"),
                     hr(),
                     fluidRow(
                       column(width = 6, h4(tags$b("Options"))),
                       column(
                         width = 6,
                         align = "right",
                         actionButton(
                           "sum_help",
                           label = "Help",
                           icon = icon("question-circle")
                         )
                       )
                     ),
                     
                     
                     checkboxInput("weights",
                                   "Use weights (strongly recommended)?",
                                   value = TRUE),
                     
                     checkboxInput("overall",
                                   "Include overall risk of bias?",
                                   value = TRUE),
                     
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
                         ".png" = "png",
                         ".jpeg" = "jpeg",
                         ".tiff" = "tiff",
                         ".eps" = "eps"
                       )
                     ),
                     
                     downloadButton("downloadsummaryplot", "Download plot"),
                     hr(),
                     p(
                       "If you use",
                       em("robvis"),
                       "to produce figures for your publication, please remember to cite the tool. A citation can be found in the \"About\" tab."
                     )
                     
                     
                   ),
                   
                   mainPanel(withSpinner(plotOutput("summaryplot")))
                   
                 )
               )
             )),
    
    # About Page =======================================================
    tabPanel(
      "About",
      value = "about",
      
      includeMarkdown("text/about.md"),
      
      h3("Citing the tool"),
      p(
        "If you use",
        em("robvis"),
        "to create risk-of-bias plots for your publication, please cite the tool using:"
      ),
      tags$ul(
        tags$li(
          "Luke A McGuinness (2019). robvis: An R package and web application for visualising risk-of-bias assessments. https://github.com/mcguinlu/robvis"
        )
      ),
      
      downloadButton("downloadbib", "Download .bib citation"),
      downloadButton("downloadris", "Download .ris citation"),
      br(),
      br(),
      includeMarkdown("text/funding.md"),
      img(src = "nihr_logo.jpg", align = "centre"),
      
      br(),
      
      includeMarkdown("text/acknowledgements.md"),
      br(),
      br()
    )
  )
)



###########################################################################
# Server ------------------------------------------------------------------
###########################################################################
server <- function(session, input, output) {
  output$image <- renderImage({
    return(list(src = "www/ROB2_plot.png",
                contentType = "image/png"))
  }, deleteFile = FALSE)
  
  
  # Navigation --------------------------------------------------------------
  
  observeEvent(input$tool, {
    if (input$tool != '') {
      show("test")
    }
    reset(id = 'data')
    hide(selector = "#mytabsetpanel li a[data-value=plots]")
    hide("new_row")
    rv$data <- NULL
    rv$warnings <- NULL
  })
  
  observeEvent(input$enter_manually, {
    show("new_row")
    reset(id = 'data')
  })
  
  observeEvent(input$gotoabout, {
    updateTabsetPanel(session, inputId = 'mytabsetpanel', selected = 'about')
  })
  
  observeEvent(input$gotodata, {
    updateTabsetPanel(session, inputId = 'mytabsetpanel', selected = 'data')
  })
  
  observeEvent(input$data, {
    hide("new_row")
  })
  
  observe({
    waiter_hide()
    hide(selector = "#mytabsetpanel li a[data-value=plots]")
    hide("new_row")
  })
  
  # On search, show results tab and move to it
  observeEvent(input$gen_plots, {
    show(selector = "#mytabsetpanel li a[data-value=plots]")
    updateTabsetPanel(session, inputId = 'mytabsetpanel', selected = 'plots')
  })
  
  
  observeEvent(input$reset, {
    reset(id = 'data')
    hide(selector = "#mytabsetpanel li a[data-value=plots]")
    hide("new_row")
    hide("test")
    reset(id = 'data')
    updateSelectInput(session = session,
                      inputId = "tool",
                      selected = "")
    rv$data <- NULL
  })
  
  
  # Help --------------------------------------------------------------------
  
  observeEvent(input$tool_help, {
    showModal(
      modalDialog(
        title = h3(strong("Choosing your tool")),
        includeMarkdown("text/help_tool.md"),
        easyClose = TRUE,
        size = "l",
        footer = modalButton("Dismiss")
      )
    )
  })
  
  observeEvent(input$data_help, {
    showModal(
      modalDialog(
        title = h3(strong("Uploading your data")),
        includeMarkdown("text/help_data.md"),
        div(align = "center", tableOutput('rob2table')),
        includeMarkdown("text/help_data2.md"),
        easyClose = TRUE,
        size = "l",
        footer = modalButton("Dismiss")
      )
    )
  })
  
  
  observeEvent(input$tf_help, {
    showModal(
      modalDialog(
        title = "",
        includeMarkdown("text/help_tf.md"),
        easyClose = TRUE,
        size = "l",
        footer = modalButton("Dismiss")
      )
    )
  })
  
  observeEvent(input$sum_help, {
    showModal(
      modalDialog(
        title = "",
        includeMarkdown("text/help_sum.md"),
        easyClose = TRUE,
        size = "l",
        footer = modalButton("Dismiss")
      )
    )
  })
  
  
  # Download handlers -------------------------------------------------------
  
  #Download datasets
  output$downloadROB2Data <- downloadHandler(
    filename = function() {
      paste("ROB2_example", ".xlsx", sep = "")
    },
    content = function(file) {
      rio::export(robvis::data_rob2, file, row.names = FALSE)
    }
  )
  
  output$downloadROBINSData <- downloadHandler(
    filename = function() {
      paste("ROBINS_example", ".xlsx", sep = "")
    },
    content = function(file) {
      rio::export(robvis::data_robins, file, row.names = FALSE)
    }
  )
  
  output$downloadQUADASData <- downloadHandler(
    filename = function() {
      paste("QUADAS_example", ".xlsx", sep = "")
    },
    content = function(file) {
      rio::export(robvis::data_quadas, file, row.names = FALSE)
    }
  )
  
  output$downloadROB1Data <- downloadHandler(
    filename = function() {
      paste("Generic_example", ".xlsx", sep = "")
    },
    content = function(file) {
      rio::export(robvis::data_rob1, file, row.names = FALSE)
    }
  )
  
  output$downloadbib <- downloadHandler(
    filename = function() {
      paste("citation", ".bib", sep = "")
    },
    content = function(file) {
      file.copy("www/robviscitation.bib", file)
    }
  )
  
  output$downloadris <- downloadHandler(
    filename = function() {
      paste("citation", ".ris", sep = "")
    },
    content = function(file) {
      file.copy("www/robviscitation.ris", file)
    }
  )
  
  output$rob2table <- renderTable(head(robvis::data_rob2, 4))
  
  
  # Data handling -----------------------------------------------------------
  
  rv <- reactiveValues()
  
  
  observe({
    if (input$tool == "ROB2") {
      rv$values = c("High", "Some concerns", "Low", "No information")
      rv$domain_text = "8: a \"Study\" column, 5 \"Domain\" columns, an \"Overall\" column, and a \"Weight\" column."
    }
    
    if (input$tool == "ROBINS-I") {
      rv$values = c("Critical",
                    "Serious",
                    "Moderate",
                    "Low",
                    "No information")
      rv$domain_text = "10: a \"Study\" column, 7 \"Domain\" columns, an \"Overall\" column, and a \"Weight\" column."
      
    }
    
    if (input$tool == "QUADAS-2") {
      rv$values = c("High", "Some concerns", "Low", "No information")
      rv$domain_text = "7: a \"Study\" column, 4 \"Domain\" columns, an \"Overall\" column, and a \"Weight\" column."
      
    }
    
    if (input$tool == "Generic") {
      rv$values = c("High", "Unclear", "Low", "No information")

    }
    
    
  })
  
  
  output$data_text <- renderText({
    paste0("Using tool template: ", input$tool)
  })
  
  
  # Data input warnings -----------------------------------------------------
  
  output$warnings_header <- renderText({
    if (!is.null(rv$warnings) && length(rv$warnings) > 0) {
      "The following errors were identified in your data. Once you have addressed them, the \"Generate Plots\" button will become active, and you will be able to proceed."
    }
    
  })
  
  observe({
    if ((is.null(rv$warnings) ||
         length(rv$warnings) == 0) && length(rv$data)[1] > 0) {
      shinyjs::enable("gen_plots")
    } else {
      shinyjs::disable("gen_plots")
    }
  })
  
  # Duplicated study names
  observe({
    if (length(rv$data[, 1]) != length(unique(rv$data[, 1]))) {
      rv$warnings$duplicate_studies <-
        "WARNING: Duplicated study names. Please ensure that each study has a unique name (e.g. \"Higgins, 2019a\" and \"Higgins, 2019b\")."
    } else {
      rv$warnings$duplicate_studies <- NULL
    }
  })
  
  output$duplicate_studies <- renderText({
    rv$warnings$duplicate_studies
  })
  
  # Wrong number of columns
  observe({
    req(input$tool)
    req(rv$data)
    
    if ((input$tool == "ROB2" &&
         ncol(rv$data) != 8) |
        (input$tool == "ROBINS-I" &&
         ncol(rv$data) != 10) |
        (input$tool == "QUADAS-2" && ncol(rv$data) != 7)) {
      rv$warnings$wrong_ncol <-
        paste0(
          "WARNING: Incorrect number of columns. The number of columns in your data does not match the number expected for the ",
          input$tool,
          " tool.",
          " The expected number of domains for this tool is ",
          rv$domain_text,
          " Please click the question mark beside \"Load data\" for more information on the number of domains expected for each tool."
        )
    } else {
      rv$warnings$wrong_ncol <- NULL
    }
    
  })
  
  output$wrong_ncol <- renderText({
    rv$warnings$wrong_ncol
  })
  
  # Wrong levels of bias
  
  
  observe({
    req(input$tool)
    req(rv$data)
    
    if (!is.na(all(unlist(
      lapply(2:as.numeric(ncol(rv$data) - 1), function(i)
        match(rv$data[, c(i)], rv$values))
    ))) == FALSE) {
      rv$warnings$wrong_levels <-
        paste0(
          "WARNING: Invalid judgement. At least one cell contains an inappropriate level of judgement for the ",
          input$tool,
          " tool.",
          " Acceptable levels of judment are: ",
          paste(rv$values, collapse = ", "),
          "."
        )
    }
    else {
      rv$warnings$wrong_levels <- NULL
    }
    
  })
  
  output$wrong_levels <- renderText({
    rv$warnings$wrong_levels
  })
  
  
  observeEvent(input$enter_manually, {
    if (input$tool == "ROB2") {
      rv$data = data.frame(
        Study   = "Click to edit",
        D1      = "Click to edit",
        D2      = "Click to edit",
        D3      = "Click to edit",
        D4      = "Click to edit",
        D5      = "Click to edit",
        Overall = "Click to edit",
        Weights = 1,
        stringsAsFactors = FALSE
      )
      rv$new_row = rv$data
    }
    
    if (input$tool == "ROBINS-I") {
      rv$data = data.frame(
        Study   = "Click to edit",
        D1      = "Click to edit",
        D2      = "Click to edit",
        D3      = "Click to edit",
        D4      = "Click to edit",
        D5      = "Click to edit",
        D6      = "Click to edit",
        D7      = "Click to edit",
        Overall = "Click to edit",
        Weights = 1,
        stringsAsFactors = FALSE
      )
      rv$new_row = rv$data
    }
    
    if (input$tool == "QUADAS-2") {
      rv$data = data.frame(
        Study   = "Click to edit",
        D1      = "Click to edit",
        D2      = "Click to edit",
        D3      = "Click to edit",
        D4      = "Click to edit",
        Overall = "Click to edit",
        Weights = 1,
        stringsAsFactors = FALSE
      )
      rv$new_row = rv$data
    }
    
  })
  
  
  
  
  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
  # Data upload -------------------------------------------------------------
  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
  
  # Capture data input and load it to the rv$data reactive value
  observeEvent(input$data, {
    tryCatch({
    
      # Use rio::import to detect and handle varying file extensions
      tmp <- rio::import(input$data$datapath)
      
      # Perform data cleaning if the number of columns is 1
      # This indicates a common problem with the saving of the legacy example
      # datasets - all data is passed into a single column and quoted
       
      if (dim(tmp)[2] == 1) {
        if (input$tool == "ROBINS-I") {
          tmp <-
            tidyr::separate(
              data = test,
              col = 1,
              into = c(
                "Study",
                "D1",
                "D2",
                "D3",
                "D4",
                "D5",
                "D6",
                "D7",
                "Overall",
                "Weight"
              ),
              sep = ","
            )
        }
        
        if (input$tool == "ROB2") {
          tmp <-
            tidyr::separate(
              data = test,
              col = 1,
              into = c(
                "Study",
                "D1",
                "D2",
                "D3",
                "D4",
                "D5",
                "Overall",
                "Weight"
              ),
              sep = ","
            )
        }
        
        if (input$tool == "QUADAS-2") {
          tmp <-
            tidyr::separate(
              data = test,
              col = 1,
              into = c("Study", "D1", "D2", "D3", "D4", "Overall", "Weight"),
              sep = ","
            )
        }
        
        # Replace qoutation marks with nothing
        for (col in 2:ncol(tmp)) {
          tmp[, col] <- gsub("\"", "", tmp[, col])
          
        }
      }
      
      rv$data <- tmp
    },
    error = function(e) {
      stop(safeError(paste('Cannot read data file')))
    })
  })
  
  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
  # Editable datatable ------------------------------------------------------
  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
  
  # Define basic editable datatable
  output$mytable <- DT::renderDataTable(
    rv$data,
    editable = list(target = 'cell'),
    server = TRUE,
    rownames = FALSE,
    escape = FALSE,
    options = list(
      dom = 't',
      ordering = F,
      columnDefs = list(list(
        className = 'dt-center', targets = "_all"
      )),
      paging = FALSE
    )
  )
  
    # Define table proxy
  proxy = dataTableProxy('mytable')
  
  # Add new row to the table
  observeEvent(input$new_row, {
    rv$data <- rbind(rv$data, rv$new_row)
    replaceData(proxy,
                rv$data,
                resetPaging = FALSE,
                rownames = FALSE)  # important
    
  }, ignoreNULL = TRUE)
  
  # Update reactive dataset on cell edit
  observeEvent(input$mytable_cell_edit, {
    info <- input$mytable_cell_edit
    i <- info$row
    j <- info$col + 1L  # column index offset by 1
    v <- info$value
    
    # Validate that the edit is one of the acceptable judgements for the tool
    isolate(if (j != 1 & j != dim(rv$data)[2]) {
      if (v %in% rv$values) {
        rv$data[i, j] <- coerceValue(v, rv$data[i, j])
        
      } else {
        shinyalert("Warning", paste0(
          c(
            "This is not a valid input. \n Valid inputs for this cell are :",
            paste(rv$values, collapse = ", ")
          ),
          collapse = " "
        ),
        type = "error")
      }
    } else {
      rv$data[i, j] <- coerceValue(v, rv$data[i, j])
    })
    
    replaceData(proxy,
                rv$data,
                resetPaging = FALSE,
                rownames = FALSE)  # important
  })
  
  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
  # Summary plot ------------------------------------------------------------
  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
  
  # Define summary plot object
  summaryplotInput <- reactive({
    try(robvis::rob_summary(
      data = rv$data,
      tool = input$tool,
      overall = input$overall,
      weighted = input$weights,
      colour = input$barplotcolour
    ))
  })
  
  # Define waitress (progress bar)
  waitress_sum <-
    Waitress$new("#summaryplot",
                 hide_on_render = TRUE,
                 infinite = TRUE)
  
  # Render summary plot
  output$summaryplot <- renderPlot({
    waitress_sum$start()
    summaryplotInput()
  })
  
  # Download handler for the summary plot
  output$downloadsummaryplot <- downloadHandler(
    filename = function() {
      paste0(input$tool, ".", input$summarydownloadformat)
    },
    content = function(file) {
      shiny::withProgress(message = paste0("Downloading ", input$tool, " figure"),
                          value = 0,
                          {
                            shiny::incProgress(7 / 10)
                            ggplot2::ggsave(
                              file,
                              plot = summaryplotInput(),
                              device = input$summarydownloadformat,
                              width = 8,
                              height = 2.41,
                              dpi = 800
                            )
                          })
    }
  )
  
  
  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
  # Traffic light plot ------------------------------------------------------
  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
  
  # Define TF plot object ----
  trafficlightplotInput <- reactive({
    robvis::rob_traffic_light(
      data = rv$data,
      tool = input$tool,
      colour = input$trafficcolour,
      psize = input$psize
    ) +
      ggplot2::theme(
        strip.text.x = ggplot2::element_text(size = input$traffictextsize),
        strip.text.y.left = ggplot2::element_text(size = input$traffictextsize),
        axis.title.x = ggplot2::element_text(size = input$traffictextsize),
        axis.title.y = ggplot2::element_text(size = input$traffictextsize)
      )
    
  })
  
  # Define TF output for on-screen ----
  # Define waitress (progress bar) - important for large datasets
  waitress_tf <-
    Waitress$new("#trafficlightplot",
                 hide_on_render = TRUE,
                 infinite = TRUE)
  
  # Output plot
  output$trafficlightplot <- renderPlot({
    waitress_tf$start()
    trafficlightplotInput()
  })
  
  # Define height of onscreen plot dynamically
  nrowspx <- reactive({
    req(input$tool)
    trafficdf <- rv$data
    nrows <- nrow(trafficdf)
    nrows <-
      ifelse(nrows * (70 / (20 / input$psize)) + 100 < 750, 750, nrows * (70 /
                                                                            (20 / input$psize)))
    return(nrows)
  })
  

  
  # Output UI element using the dynamic height
  output$trafficplotUI <- renderUI({
    plotOutput("trafficlightplot", height = nrowspx())
  })
  
  
  # Define TF for download ----
  # Define height of downloaded plot dynamically
  nrowsin <- reactive({
    trafficdf <- rv$data
    nrows <- nrow(trafficdf)
    nrows <-
      ifelse(nrows * 1 / (20 / input$psize) < 9, 9, nrows * 1 / (20 / input$psize))
    return(nrows)
  })
  
  # TF download function
  output$downloadtrafficlightplot <- downloadHandler(
    filename = function() {
      paste0(input$tool, ".", input$trafficdownloadformat)
    },
    content = function(file) {
      shiny::withProgress(message = paste0("Downloading ", input$tool, " figure"),
                          value = 0,
                          {
                            shiny::incProgress(7 / 10)
                            ggplot2::ggsave(
                              file,
                              plot = trafficlightplotInput(),
                              device = input$trafficdownloadformat,
                              width = (8 - (2 * (
                                1 / (20 / input$psize)
                              ))),
                              # width = (9 - (100/(80+input$psize))),
                              height = nrowsin(),
                              units = "in",
                              dpi = 800,
                              limitsize = FALSE
                            )
                          })
    }
  )
}

###########################################################################
# Call application --------------------------------------------------------
###########################################################################
shinyApp(ui = ui, server = server)
