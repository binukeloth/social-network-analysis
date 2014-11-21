Social Network Analysis using Calls (CDR)
=======================

Project to identify interesting properties from social network.

Funcitonalities
---------------
  * Loading data file in csv format with calls data
    * data should have columns to and from
    * data filter based on other columns
  * Generating properties
    * node - degree, betweenness, closeness, hub, authority
    * edge - edgeWeight
    * graph - 
  * Identifying communities
  * Subsetting/Filter based on community
  * Saving graph file
  
Definitions
-----------

### Modularity:
  The simplest definition of modularity for an undirected graph, i.e. the adjacency matrix A is   symmetric, 
  
  Q(A) = 1/W * Sum over all communities( Sum over all nodes i,j [Aij - ((ki * kj) / W)])

  where W = Sum over i,j Aij and ki = Sum over j Aij, is the degree of node i. 
  The indices i and j run over the N nodes of the graph G. The index C runs over the communities of   the partition P. 
  Modularity counts the number of links between all pairs of nodes belonging to the same community   and compares it to the expected number of such links for an equivalent random graph in which the   degree of all nodes has been left unchanged.


_By construction |Q| ≤ 1 with larger Q indicating that more links remain within communities then would be expected in the random model._

### Community finding using modularity
Research paper - _Finding community structure in very large networks_ - Aaron Clauset M. E. J. Newman,2 and Cristopher Moore1,


References
----------
  * [Supported Graph Formats](http://gephi.github.io/users/supported-graph-formats/)