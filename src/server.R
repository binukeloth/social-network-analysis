#File upload size to 30MB
options(shiny.maxRequestSize=30*1024^2, digits=6)

library(shiny)
library(datasets)
source("network-analysis.R");

workDir = "D:/WorkSpace/R/SNA/data/";

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
    #print("datasetInput invoked");
    
    dataFile <- input$dataFile
    
    if (is.null(dataFile))
      return(NULL)		
    
    loadData(dataFile$datapath, input$header, input$sep);
  })  
  
  # Show the first "n" observations
  output$view <- renderTable({
    print("renderTable invoked");
    dataset = datasetInput();    
    head(dataset, n = input$obs)
  })
  
  # Server side for nw summary stats tab
  # --------------------------
  
  output$condition = renderText ({ 
    input$cond;
  })
  
  # Read the GML file  
  graphFromFile <- reactive({
    print("graphFromFile invoked");
    dataFile = input$nwFile
      
    if (is.null(dataFile))
      return(NULL)    
      
    read.graph(dataFile$datapath, "gml");
  })
  
  # Generate graph from raw data
  graphFromData = reactive ({
    print("graphFromData invoked");
    dataset = datasetInput();
    
    if (is.null(dataset))
      return(NULL)    
   
    genGraph(dataset, NULL);    
  })
  
  # Generating graph in one of 3 ways
  #   1. From already loaded file
  #   2. From network file GML
  #   3. Subgraph based on community
  
  graphDetails = reactive({
    
    print("graphDetails invoked");
    
    is.subGraph = input$analyze;    
    if(is.subGraph > 0)
    {
      sub.g = graphFromData();
      if(is.null(sub.g) == TRUE)
        return(NULL);      
      
      nw = findCommunity(computeNWProp(sub.g));
      
      nodeProps = data.frame(vertex.attributes(nw$graph));
      com.sub = nodeProps[nodeProps$Membership == input$analyzeID,];      
      
      if(nrow(com.sub) > 1) {
        com.sub.g = induced.subgraph(nw$graph, com.sub$name);        
        return(computeNWProp(com.sub.g))
      }
      else {
        print("Not enough nodes to form graph. Please use another community");
        return(NULL);
      }
    }
    
    # Graph from file option is populated 
    # This take priority
    gf = graphFromFile();    
    if(is.null(gf) == FALSE)
      return(computeNWProp(gf));
    
    # graph from Data if popultaed from "Data View"
    gd = graphFromData();
    if(is.null(gd) == FALSE)
      return(computeNWProp(gd));
    
    return(NULL);
    
#     fromCurrentNW = input$analyze;
#     
#     #print(fromCurrentNW);    
#     
#     if(is.null(fromCurrentNW) || (fromCurrentNW == 0)) {      
#        
#     }
#     else {
# 
# #       g = graphDetails();
# #       nw = findCommunity(g);    
# #       str(nw);
# #       str(nw);
# #       nodeProps = data.frame(vertex.attributes(nw$graph));
# #       com.sub = nodeProps[nodeProps$Membership == input$comID,];
#       
# #       if(nrow(com.sub) > 1) {
# #         com.sub.g = induced.subgraph(g$graph, com.sub$node);
# #         computeNWProp(com.sub.g);
# #       }
# #       else {
# #         print("Not enough nodes to form graph. Please use another community");
# #         return(NULL);
# #       }
#     }
  })
  
  # Saving graphs and properties
  saveGraph = reactive({  
    # TODO - Get the actual file directory
    if(is.null(input$dataFile) == FALSE){
      in.file = input$inputFile$datapath;
    }
    else if(is.null(input$dataFile) == FALSE){
      in.file = input$nwFile$datapath;
    }
    
    in.file = paste0(workDir, input$dataFile$name);        
    
                       
    if(input$save > 0) {
      g = graphDetails();
      out.file = getOutputFile(in.file, input$save) ;
      print(paste0("Saving graph to file - ",out.file));
            
      writeGraph(g$graph, paste0(out.file, '.gml'));
      writeGraphProps(g$graph, out.file)
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
    nw = computeCommunity();
    printNWSummary(nw);  
    
    saveGraph();
  })
  
  
  # Server side for communities
  # -----------------------------
  computeCommunity = reactive({
    print("Community Invoked");
    g = graphDetails();
    nw = findCommunity(g);    
  })  
  
  output$comTop = renderPrint({    
    nw = computeCommunity();
    
    nodeProps = data.table(membership = get.vertex.attribute(nw$graph, "Membership", index=V(nw$graph)));
    
    setkey(nodeProps, membership);
    comSummary = nodeProps [,list(cnt=.N), by="membership"];    
    propOrder = order(comSummary[, cnt], decreasing = TRUE);    
    cat(paste0("Top 10 communities based on members:\n"));
    print(head(comSummary[propOrder,], 10));      
  })
  
  output$comPlot <- renderPlot({    
    if(input$plotGraph == FALSE){
      return (NULL);
    }
    
    nw = computeCommunity();
    
    plotCommGraph(nw$graph, nw$community);  
    #plot(g$community, g$graph);
  })
  
  output$comDetails = renderPrint ({
    nw = computeCommunity();
    
    if(input$comID != "") {
      nodeProps = data.frame(vertex.attributes(nw$graph));
      nodeProps[nodeProps$Membership == input$comID,];
    }
    else {
      nw$community;
    }
  })
  
  # Server side for Omega
  # ---------------------
  
  #   output$statsTable <- renderPrint({
  #     g = graphDetails();
  #     
  #     if(input$node == "") {    
  #       head(g$nodeProps, n = input$obs)
  #     }
  #     else
  #     {
  #       head(g$nodeProps[g$nodeProps[,eval(g$nodeProps$node == input$node)],], n = input$obs)      
  #     }      
  #   })
 
})
