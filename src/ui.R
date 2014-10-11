library(shiny)

# Define UI for dataset viewer application
shinyUI(
  navbarPage(theme = "bootstrap.min.css", "Social Network Analysis",
      tabPanel("Load Data",                       
              sidebarPanel(
                        fileInput('file1', 'Choose Data File',
                                  accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                        tags$hr(),
                        checkboxInput('header', 'Header', TRUE),
                        radioButtons('sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), ','),
                        numericInput("obs", "Observations to view:", 10)
              ),
                      
              # Show a summary of the dataset and an HTML table with the 
              # Requested number of observations
              mainPanel(
                        tableOutput("view")
              )
          ),
      tabPanel("Summary Stats",
              sidebarPanel(
                  h4("Filters"),
                  tags$hr(),                  
                  p("Provide the subsetting condition on columns available on the data :"),
                  textInput("cond", label ="", value = "Enter condition..."),
                  tags$hr(),                  
                  p("Filter based on date range"),
                  dateRangeInput("daterange", label = "", start = NULL, end = NULL),
                  #p(strong("Subset Condition : "), verbatimTextOutput("condition"))
                  actionButton("saveGraph", label="Save Graph")
                ),
                      
              mainPanel(                        
                        #textOutput("condition"),
                        #tags$hr(),
                        h4("Network plot"),
                        div(plotOutput("nwPlot")),
                        tags$hr(),
                        h4("Properties of the Network"),
                        div(verbatimTextOutput("summary"))                        
                  )                 
               ),
      tabPanel("Stats Table",
                      sidebarPanel(
                        h4("Filters"),
                        tags$hr(),                                          
                        textInput("node", label ="Enter Node Id", value = "")
                      ),                      
                      mainPanel(                        
                        verbatimTextOutput("statsTable")          
                      )                 
             ),         
      tabPanel("Compare Networks",
               sidebarPanel(
                 p("Enter network")),
               sidebarPanel(
                 p("Enter network"))
              )
        )
  )

