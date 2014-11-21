library(shiny)

#File upload size to 30MB
options(shiny.maxRequestSize=30*1024^2, digits=6)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  navbarPage(theme = "bootstrap.min.css", "Social Network Analysis",
     tabPanel("Data View",
        fluidRow(
          column(3,
            fileInput('dataFile', 'Choose Data File',
                      accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),                 
            hr(),
            h4("CSV Load Options"),
            checkboxInput('header', 'Header', TRUE),
            radioButtons('sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), ','),
            numericInput("obs", "Observations to view:", 20),                 
            hr(),
            h4("Data Filters"),                                          
            helpText("Provide the subsetting condition on columns available on the data :"),
            textInput("cond", label ="", value = "Enter condition..."),                        
            helpText("Filter based on date range"),
            dateRangeInput("daterange", label = "", start = NULL, end = NULL)
            #p(strong("Subset Condition : "), verbatimTextOutput("condition"))                
          ),          
        # Show a summary of the dataset and an HTML table with the 
        # Requested number of observations
          column(9,
              h4("Data View"),
              tableOutput("view")
          )
     )),
    tabPanel("Network View",
      fluidRow(
        column(3,
           fileInput('nwFile', 'Choose Network File',accept=c('gml', 'Geography Markup Language', '.gml')),
            hr(),
            checkboxInput("plotGraph", label = "Plot Graph", value = FALSE),
            hr(),            
            helpText("Save graph stores graph and all the node and edge properties data file location."),
            actionButton("save", label="Save Graph"),
            hr(),
            textInput("analyzeID", label ="Enter Community Id", value = ""),
            helpText(paste0("Analyze subsets current network based ",
                           "on mentioned community id and generates properties for the subnetwork. ",
                            "Henceforth all operations happens on this subnetwork ")),            
            actionButton("analyze", label="Analyze Community")  
        ),              
        column(9,                       
            #textOutput("condition"),
            #hr(),
            h4("Properties of the Network"),
            div(verbatimTextOutput("summary")),
            hr(),
            h4("Network plot"),
            div(plotOutput("nwPlot"))              
          )
     )), 
     tabPanel("Community",
      fluidRow(
        column(3,
            h4("Filters"),            
            textInput("comID", label ="Enter Community Id", value = "")
          ),                      
        column(9,
            verbatimTextOutput("comTop"),                      
            hr(),
            h4("Community Details"),
            verbatimTextOutput("comDetails"),
            hr(),
            h4("Community plot"),
            div(plotOutput("comPlot"))
          )
     )),
    tabPanel("Omega Insights",
        sidebarPanel(
          h4("Filters")          
        ),                      
        mainPanel(                        
          verbatimTextOutput("statsTable")          
        )                 
    )        
  )
))
