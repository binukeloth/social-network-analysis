#RScript to load data file and generate iGraph
library(igraph)
library(data.table)

stringsAsFactors=FALSE

# Read in raw data
# PreContition - csv file with node connections
#   field 1 - originating node; mandatory
#   field 2 - Terminating node; mandatory
#   date of event: Optional
#   other optional columns for subsetting

loadData = function(dataFile, header = TRUE, sep = ',') {
  data.table(read.csv(file=dataFile, header=header, sep=","));
}

computeNWProp = function (dataTable, subsetCond) {
  setkey(dataTable, "from", "to");  
  
  
  if(is.null(subsetCond))
  {
    nw.data.sum = dataTable[ ,list("edgeWeight"=.N), by="from,to"];
  }
  else
  {
    nw.data.sum = dataTable[eval(subsetCond), 
                            list("edgeWeight"=.N), by="from,to"];
  }
  
  nw.graph = graph.data.frame(nw.data.sum);
  #set.edge.attribute(nw.graph, "EdgeWeight", index=E(nw.graph), nw.data.sum[3,])
  nw.com = findCommunity(nw.graph);
  nw.mem = membership(nw.com);
  
  nw.nodeProps = data.table(node = V(nw.graph)$name, degree = degree(nw.graph), 
                        betweeness = betweenness(nw.graph, directed=TRUE),
                        closeness = closeness(nw.graph),
                        membership = nw.mem[V(nw.graph)$name]);
  
  
  nw.edgeProps = data.table(edge=E(nw.graph));
  
 
  nw.props = list("degDist" = degree.distribution(nw.graph), 
                  "density" = graph.density(nw.graph, loops=TRUE),
                  "modularity" = modularity(com));    
  
  
  # tables();  
  nw = list("graph"= nw.graph,
               "props" = nw.props,
               "nodeProps"= nw.nodeProps,
               "edgeProps" = nw.edgeProps,
              "community" = com);
  
  #print.summary.nw(nw);

  return(nw);
}


findCommunity = function (graph) {
  com = fastgreedy.community(as.undirected(graph));
                             
}

writeGraph = function(graph, outFile) {
  
  write.graph(graph, outFile, format = "gml");
}

getOutputFile = function(dataFile) {
  tmp.outputFile = strsplit(dataFile, "\\.")[[1]][1];
  tmp.outputFile = paste0(tmp.outputFile, ".gml");
  
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
    
    invisible(nw);
}

# c = edge.betweenness.community(g$graph)

# Usage as application
# --------------------
# dataFile = "D:/WorkSpace/R/SNA/data/samplecdrs.csv"
#
# source("D:/WorkSpace/R/SNA/src/network-analysis.R")
# g = computeNWProp(loadData(dataFile), quote(call_type == 3))
# plotGraph(g$graph)
# writeGraph(g$graph, getOutputFile(dataFile))
