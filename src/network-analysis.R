#RScript to load data file and generate iGraph
library(igraph)
library(data.table)
library(rgexf)

#File upload size to 30MB
options(shiny.maxRequestSize=30*1024^2, digits=6)
stringsAsFactors=FALSE

# Read in raw data
# PreContition - csv file with node connections
#   field 1 - originating node; mandatory
#   field 2 - Terminating node; mandatory
#   date of event: Optional
#   other optional columns for subsetting

loadData = function(dataFile, header = TRUE, sep = ',') {
  data.table(read.csv(file=dataFile, header=header, sep=",", stringsAsFactors=FALSE));
}

genGraph = function(dataTable, subsetCond = NULL, metaVertices = NULL) {
  setkey(dataTable, "from", "to");  
  
  if(is.null(subsetCond))
  {
    nw.data.sum = dataTable[ , list("edgeWeight"=.N), by="from,to"];
  }
  else
  {
    # list("edgeWeight"=.N)
    nw.data.sum = dataTable[eval(subsetCond), list("edgeWeight"=.N), by="from,to"];
  }
  
  nw.graph = graph.data.frame(d = nw.data.sum, directed=TRUE, vertices=metaVertices);
}


computeNWProp = function (nw.graph) {
  
  #set.edge.attribute(nw.graph, "EdgeWeight", index=E(nw.graph), nw.data.sum[3,]);
  
  nw.graph = set.vertex.attribute(nw.graph, name = "Degree",index = V(nw.graph), value = degree(nw.graph));
  nw.graph = set.vertex.attribute(nw.graph, name = "InDegree",index = V(nw.graph), value = degree(nw.graph, mode="in"));
  nw.graph = set.vertex.attribute(nw.graph, name = "OutDegree",index = V(nw.graph), value = degree(nw.graph, mode="out"));
  nw.graph = set.vertex.attribute(nw.graph, name = "Betweenness",index = V(nw.graph), value = betweenness(nw.graph));
  nw.graph = set.vertex.attribute(nw.graph, name = "Closeness",index = V(nw.graph), value = closeness(nw.graph));
  nw.graph = set.vertex.attribute(nw.graph, name = "Authority",index = V(nw.graph), value = authority.score(nw.graph)$vector);
  nw.graph = set.vertex.attribute(nw.graph, name = "Hub",index = V(nw.graph), value = hub.score(nw.graph)$vector);
  
  nw.graph = set.graph.attribute(nw.graph, name="DegDist", degree.distribution(nw.graph));
  nw.graph = set.graph.attribute(nw.graph, name="Density", graph.density(nw.graph, loops=TRUE));
  
  # tables();  
  nw = list("graph"= nw.graph);          #"community" = nw.com);
  #print.summary.nw(nw);
  return(nw);
}

# Trimming leading and traling spaces
trim <- function (x) gsub("\\s+", "", x);

# Finding communities
findCommunity = function (nw, method = "fastgreedy") {
  
  if(method == "edge.betweenness") { 
    nw.com = edge.betweenness.community(nw$graph);
  }
  else
    nw.com = fastgreedy.community(as.undirected(nw$graph));  
    
  nw.mem = membership(nw.com);  
  
  # Set properties to network, node and graph
  nw$graph = set.vertex.attribute(nw$graph, name = "Membership",
                                  index = V(nw$graph), value = nw.mem[V(nw$graph)$name]);
  nw$graph = set.graph.attribute(nw$graph, name="Modularity", modularity(nw.com));
  nw$community = nw.com;
  
  return(nw);
}

writeGraph = function(graph, outFile) {  
  write.graph(graph, outFile, format = "gml");  
}

writeGraphProps = function(graph, outFileStub) {    
  write.csv(vertex.attributes(graph), paste0(outFileStub, "-node.csv"));
  write.csv(edge.attributes(graph), paste0(outFileStub, "-edge.csv"));
}

writeGraphRgexf = function (nw, outFile) {  
  gD= nw$graph;
  
  # Create a dataframe nodes: 1st column - node ID, 2nd column -node name
  nodes_df <- data.frame(ID = c(1:vcount(gD)), NAME = V(gD)$name)
  # Create a dataframe edges: 1st column - source node ID, 2nd column -target node ID
  edges_df <- as.data.frame(get.edges(gD, c(1:ecount(gD))))
  
  # Define node and edge attributes - these attributes won't be directly used for network visualization, but they
  # may be useful for other network manipulations in Gephi
  #
  # Create a dataframe with node attributes
  nodes_att <- nw$nodeProps;
  #
  # Create a dataframe with edge attributes: 1st column - attribute 1 (weight), 2nd column - attribute 2 (similarity)
  edges_att <- nw$edgeProps;
  
  write.gexf(nodes = nodes_df, edges = edges_df, 
          nodesAtt = nodes_att, output = outFile)
}

getOutputFile = function(dataFile, partNo) {  
  tmp.outputFile = strsplit(dataFile, "\\.")[[1]][1];
  tmp.outputFile = paste0(tmp.outputFile,"-" ,partNo);
  
  return(tmp.outputFile);
}

plotGraph = function(graph) {
  #plot it
  opar <- par()$mar;
  par(mar=rep(0, 4)) #Give the graph lots of room
  
  plot.igraph(graph, layout=layout.auto(graph))
  par(mar=opar);
}

plotCommGraph = function(graph, com) {
  #plot it
  opar <- par()$mar;
  par(mar=rep(0, 4)) #Give the graph lots of room
  
  plot(com, graph);
  par(mar=opar);
  
  # With communities
  # V(g$graph)$color <- com$membership + 1
  # plot(g$graph, vertex.label.dist=1.5)
  # plot(g, layout=layout.fruchterman.reingold, vertex.size=4,
  # vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.5)
  # g <- set.graph.attribute(g, "layout", layout.kamada.kawai(g))  
}

printNWSummary = function(nw, ...) {
    topn = 3;    
    print(nw$graph);
    cat(paste0("Density = ", get.graph.attribute(nw$graph, "Density"), "\n"));
    #plot(nw$props$degDist);
    
    nodeProps = data.frame(vertex.attributes(nw$graph));
    props = c("Degree", "InDegree", "OutDegree", "Betweeness", 
              "Closeness"); #, "Authority", "Hub");
    
    for(val in props) {
      propOrder = order(nodeProps[val, ], decreasing = TRUE);
      cat(paste0("\nTop ", topn, " nodes based on ", val, ":\n"));
      print(nodeProps[head(propOrder, topn), ]);      
    }
    
    invisible(nw);
}

# Usage as application
# --------------------
# dataFile = "D:/WorkSpace/R/SNA/data/samplecdrs.csv"
#
# source("D:/WorkSpace/R/SNA/src/network-analysis.R")
# g = computeNWProp(genGraph(loadData(dataFile), NULL))
# g = computeNWProp(genGraph(loadData(dataFile), quote(call_type == 3)))
# plotGraph(g$graph)
# writeGraph(g$graph, getOutputFile(dataFile))
