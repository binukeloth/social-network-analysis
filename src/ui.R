library(shiny)

#File upload size to 30MB
options(shiny.maxRequestSize=30*1024^2, digits=6)

# Define UI for dataset viewer application
shinyUI(
  navbarPage(theme = "bootstrap.min.css", "Social Network Analysis",
             tabPanel("Load Data",                       
                      sidebarPanel(
                        fileInput('dataFile', 'Choose Data File',
                                  accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),                        
                        checkboxInput('header', 'Header', TRUE),
                        radioButtons('sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), ','),
                        numericInput("obs", "Observations to view:", 20),
                        hr(),
                        h4("Filters"),                                          
                        p("Provide the subsetting condition on columns available on the data :"),
                        textInput("cond", label ="", value = "Enter condition..."),                        
                        p("Filter based on date range"),
                        dateRangeInput("daterange", label = "", start = NULL, end = NULL)
                        #p(strong("Subset Condition : "), verbatimTextOutput("condition"))
                        
                      ),           
                      
                      # Show a summary of the dataset and an HTML table with the 
                      # Requested number of observations
                      mainPanel(
                        tableOutput("view")
                      )
             ),
             tabPanel("Network",
                      sidebarPanel(
                        fileInput('nwFile', 'Choose Network File',
                                  accept=c('gml', 'Geography Markup Language', '.gml')),
                        hr(),
                        checkboxInput("plotGraph", label = "Plot Graph", value = FALSE),
                        hr(),                      
                        textInput("comID", label ="Enter Community Id", value = ""),                          
                        actionButton("analyze", label="Analyze Community"),
                        hr(),
                        actionButton("save", label="Save Graph")
                      ),
                      
                      mainPanel(                        
                        #textOutput("condition"),
                        #hr(),
                        h4("Properties of the Network"),
                        div(verbatimTextOutput("summary")),
                        hr(),
                        h4("Network plot"),
                        div(plotOutput("nwPlot"))                                   
                      )                 
             ),
             tabPanel("Stats Table",
                      sidebarPanel(
                        h4("Filters"),
                        hr(),                                          
                        textInput("node", label ="Enter Node Id", value = "")
                      ),                      
                      mainPanel(                        
                        verbatimTextOutput("statsTable")          
                      )                 
             ),         
             tabPanel("Community",
                      sidebarPanel(
                        h4("Filters"),
                        hr(),       
                        textInput("com", label ="Enter Community Id", value = "")
                      ),                      
                      mainPanel(
                        verbatimTextOutput("comTop"),                      
                        hr(),
                        h4("Community Details"),
                        verbatimTextOutput("comDetails"),
                        hr(),
                        h4("Community plot"),
                        div(plotOutput("comPlot"))
                      )
             )
  )
)
