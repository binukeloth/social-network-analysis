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
  
  #set.edge.attribute(nw.graph, "EdgeWeight", index=E(nw.graph), nw.data.sum[3,])
  
  #nw.com = findCommunity(nw.graph);
  #nw.mem = membership(nw.com);
  
  nw.graph = set.vertex.attribute(nw.graph, name = "degree",index = V(nw.graph), value = degree(nw.graph));
  nw.graph = set.vertex.attribute(nw.graph, name = "in-Degree",index = V(nw.graph), value = degree(nw.graph, mode="in"));
  nw.graph = set.vertex.attribute(nw.graph, name = "Out-Degree",index = V(nw.graph), value = degree(nw.graph, mode="out"));
  nw.graph = set.vertex.attribute(nw.graph, name = "betweenness",index = V(nw.graph), value = betweenness(nw.graph));
  nw.graph = set.vertex.attribute(nw.graph, name = "closeness",index = V(nw.graph), value = closeness(nw.graph));
  nw.graph = set.vertex.attribute(nw.graph, name = "authority",index = V(nw.graph), value = authority.score(nw.graph)$vector);
  nw.graph = set.vertex.attribute(nw.graph, name = "hub",index = V(nw.graph), value = hub.score(nw.graph)$vector);
  
  nw.nodeProps = data.table(node = V(nw.graph)$name,
                        #company = get.edge.attribute(nw.graph, "comp", E(nw.graph)),
                        degree = degree(nw.graph),
                        indegree = degree(nw.graph, mode="in"),
                        outdegree = degree(nw.graph, mode="out"),
                        betweeness = betweenness(nw.graph, directed=TRUE),
                        closeness = closeness(nw.graph),
                        authority = authority.score(nw.graph)$vector,
                        hub = hub.score(nw.graph)$vector);
                        #membership = nw.mem[V(nw.graph)$name]);
  
  write.csv(nw.nodeProps, "D:/WorkSpace/R/SNA/data/propOut.csv");
  
  
  nw.edgeProps = data.table(edge=E(nw.graph));
          # weight = get.edge.attribute(nw.graph, "edgeWeight", E(nw.graph)));
 
  nw.props = list("degDist" = degree.distribution(nw.graph), 
                  "density" = graph.density(nw.graph, loops=TRUE));
                  #"modularity" = modularity(nw.com));
  
  # tables();  
  nw = list("graph"= nw.graph,
               "props" = nw.props,
               "nodeProps"= nw.nodeProps,
               "edgeProps" = nw.edgeProps);
               #"community" = nw.com);
  
  #print.summary.nw(nw);

  return(nw);
}

# Trimming leading and traling spaces
trim <- function (x) gsub("\\s+", "", x);


findCommunity = function (graph) {
  com = fastgreedy.community(as.undirected(graph));
  # com = edge.betweenness.community(graph);
}

writeGraph = function(graph, outFile) {  
  write.graph(graph, outFile, format = "gml");
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
  tmp.outputFile = paste0(tmp.outputFile, partNo, ".gml");
  
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
    topn = 5;
    #cat("\nProperties of the Network:\n");
    print(nw$graph);
    cat(paste0("Density = ", nw$props$density, "\n"));
    #plot(nw$props$degDist);
        
    setkey(nw$nodeProps, degree);
    cat(paste0("\nTop ", topn, " nodes based on Degree:\n"));
    print(tail(nw$nodeProps[,list(node, degree)], topn));
    
    setkey(nw$nodeProps, betweeness);
    cat(paste0("\nTop ", topn, " nodes based on betweeness:\n"));
    print(tail(nw$nodeProps[,list(node, betweeness)], topn));
    
    setkey(nw$nodeProps, closeness);
    cat(paste0("\nTop ", topn, " nodes based on closeness:\n"));
    print(tail(nw$nodeProps[, list(node, closeness)], topn));
    
    setkey(nw$nodeProps, authority);
    cat(paste0("\nTop ", topn, " nodes based on authority:\n"));
    print(tail(nw$nodeProps[, list(node, authority)], topn));
    
    setkey(nw$nodeProps, hub);
    cat(paste0("\nTop ", topn, " nodes based on hub:\n"));
    print(tail(nw$nodeProps[, list(node, hub)], topn));
    
    invisible(nw);
}
#     cat(paste0("\nTop ", topn, " nodes based on authority:\n"));
#     print(tail(nw$nodeProps[, list(node, authority)], topn));
#     
#     setkey(nw$nodeProps, hub);
#     cat(paste0("\nTop ", topn, " nodes based on hub:\n"));
#     print(tail(nw$nodeProps[, list(node, hub)], topn));
# Usage as application
# --------------------
# dataFile = "D:/WorkSpace/R/SNA/data/samplecdrs.csv"
#
# source("D:/WorkSpace/R/SNA/src/network-analysis.R")
# g = computeNWProp(genGraph(loadData(dataFile), NULL))
# g = computeNWProp(genGraph(loadData(dataFile), quote(call_type == 3)))
# plotGraph(g$graph)
# writeGraph(g$graph, getOutputFile(dataFile))
