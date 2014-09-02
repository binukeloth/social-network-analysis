#RScript to load data file and generate iGraph
library(igraph, data.table)

stringsAsFactors=FALSE

# read in raw data
# PreContition - csv file with node connections
#   field 1 - originating node; mandatory
#   field 2 - Terminating node; mandatory
#   field 3 - connection strenght; Optional; ex: number of calls, no of mails
#   field 4 - date of event; Optional


computeNWProp = function (fileName)
{
  nw.data = data.table(read.csv(file=fileName));
  setkey(nw.data, "caller", "called")
  nw.data.sum = nw.data[, list("edgeWeight"=.N), by="caller,called"]
  
  
  nw.graph = graph.data.frame(nw.data.sum);
  #set.edge.attribute(nw.graph, "EdgeWeight", index=E(nw.graph), nw.data.sum[3,])
  
  nw.nodeProps = data.table(degree = degree(nw.graph), 
                         betweeness = betweenness(nw.graph, directed=TRUE),
                         closeness = closeness(nw.graph));
  
  tables()
  #str(nw.nodeProps)
  
  return (list("graph"=nw.graph, "props"=nw.nodeProps));           
}


g = computeNWProp("C:/WorkSpace/R/SNA/data/samplecdrs.csv")
g$graph
g$props
#plot(myNW.graph)
