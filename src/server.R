#File upload size to 30MB
options(shiny.maxRequestSize=30*1024^2, digits=6)

library(shiny)
library(datasets)
source("network-analysis.R");

# Define server logic required 
# Load data set and to summarize and view the selected dataset

shinyServer(function(input, output) 
{
  # input$dataFile will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
  
  # Server side for load data tab
  # --------------------------
  
  # Read the requested dataset  
  datasetInput <- reactive({
    dataFile <- input$dataFile
    
    if (is.null(dataFile))
      return(NULL)		
    
    loadData(dataFile$datapath, input$header, input$sep);
  })
  
  # Read the GML file  
  graphFromFile <- reactive({
    dataFile <- input$nwFile
    
    if (is.null(dataFile))
      return(NULL)  	
    
    read.graph(dataFile$datapath, "gml");
  })
  
  
  # Show the first "n" observations
  output$view <- renderTable({
    dataset = datasetInput();    
    head(dataset, n = input$obs)
  })
  
  # Server side for nw summary stats tab
  # --------------------------
  
  output$condition = renderText ({ 
    input$cond;
  })
  
  graphFromData = reactive ({    
    dataset = datasetInput();
    
    if (is.null(dataset))
      return(NULL)    
   
    genGraph(dataset, NULL);    
  })
  
  graphDetails = reactive({        
    fromCurrentNW = input$analyze;
    
    #print(fromCurrentNW);    
    
    if(is.null(fromCurrentNW) || (fromCurrentNW == 0)) {      
      g = graphFromData();
      
      if(is.null(g)){
        return(NULL);
      }
        
      computeNWProp(g);      
    }
    else {
      com.sub = subset(g$nodeProps, g$nodeProps$membership == input$comID)
      
      if(nrow(com.sub) > 1) {
        g = induced.subgraph(g$graph, com.sub$node);
        computeNWProp(g);
      }
      else {
        print("Not enough nodes to form graph. Please use another community");
        return(NULL);
      }
    }
    
#     if(!is.null(input$nwFile))
#     {
#       g = graphFromFile();
#       computeNWProp(g);  
#     }    
  })
  
  saveGraph = reactive({        
    #print(input$save);    
    
    if(input$save > 0) {
      g = graphDetails();
      nw.file = "D:/WorkSpace/R/SNA/data/grapghout.gml" ;
      print(paste0("Saving graph to file - ",nw.file));
      writeGraph(g$graph, nw.file);
    }
  })

  output$nwPlot <- renderPlot({ 
    if(input$plotGraph == FALSE){
      return (NULL);
    }
      
    g = graphDetails();    
    plotGraph(g$graph);    
  })
  
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    g = graphDetails();
    
    printNWSummary(g);
    saveGraph();
  })
  
  # Server side for data table stats tab
  # ------------------------------  

  output$statsTable <- renderPrint({
    g = graphDetails();
    
    if(input$node == "") {    
      head(g$nodeProps, n = input$obs)
    }
    else
    {
      head(g$nodeProps[g$nodeProps[,eval(g$nodeProps$node == input$node)],], n = input$obs)      
    }      
  })
  
  
  # Server side for communities
  # ------------------------------  
  output$comTop = renderPrint({
    g = graphDetails();
    setkey(g$nodeProps, membership);
    comSummary = g$nodeProps [,list("cnt"=.N), by="membership"];
    setkey(comSummary, cnt);
    
    cat(paste0("\nTop 10 communities:\n"));
    print(tail(comSummary[, list(membership, cnt)], 10));
  })
  
  output$comPlot <- renderPlot({
    
    if(input$plotGraph == FALSE){
      return (NULL);
    }
    
    g = graphDetails();    
    
    plotCommGraph(g$graph, g$community);    
    #plot(g$community, g$graph);
  })
  
  output$comDetails = renderPrint ({
    g = graphDetails(); 
    
    if(input$com != "") {
      print(paste0("here ", input$com));
      subset(g$nodeProps, g$nodeProps$membership == input$com);    
    }
    else {
      g$nodeProps
    }
  })
  
})
