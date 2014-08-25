# RScript to load data file and generate iGraph
library(igraph)

# read in the .gml file
nw = read.graph(file=file.choose(),format="edgelist")
plot(nw)