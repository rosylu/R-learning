library(igraph)

# Construct an Igraph
nodes <- data.frame(names = c("A", "B", "C", "D", "E", "F", "G", "H"))
relations <- data.frame(
  from = c("B", "C", "D", "E", "F", "G", "H"), 
  to = c("A", "A", "B", "B", "C", "C", "C"))
g <- graph.data.frame(relations, directed = TRUE, vertices = nodes)

quartz()
plot(g)

# Run the page rank algorithm
pg <- page.rank(g, damping = 0.15)
round(pg$vector[sort.list(pg$vector, decreasing = TRUE,)],3)







