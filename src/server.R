library(shiny)
library(datasets)
source("network-analysis.R");

# Define server logic required 
# Load data set and to summarize and view the selected dataset

shinyServer(function(input, output) 
{
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
  
  # Server side for load data tab
  # --------------------------
  
  # Read the requested dataset	
  
  datasetInput <- reactive({
    dataFile <- input$file1
    
    if (is.null(dataFile))
      return(NULL)		
    
    loadData(dataFile$datapath, input$header, input$sep);
  })
  
  
  # Show the first "n" observations
  output$view <- renderTable({
    dataset = datasetInput();    
    head(dataset, n = input$obs)
  })
  
  # Server side for nw stats tab
  # --------------------------
  
  output$condition = renderText ({ 
    input$cond;
  })
  
  computeNW = reactive ({    
    dataset = datasetInput();
    
    if (is.null(dataset))
      return(NULL)    
   
    computeNWProp(dataset, NULL);
  })
  
  output$nwPlot <- renderPlot({    
    g = computeNW();    
    
    plotGraph(g$graph);    
  })
  
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    g = computeNW();
    
    printNWSummary(g);
  })
  
  # Server side for data table stats tab
  # ------------------------------  

  output$statsTable <- renderPrint({
    g = computeNW();
    
    if(input$node == "") {    
      head(g$nodeProps, n = input$obs)
    }
    else
    {
      head(g$nodeProps[g$nodeProps[,eval(g$nodeProps$node == input$node)],], n = input$obs)      
    }
      
  })
  
})
