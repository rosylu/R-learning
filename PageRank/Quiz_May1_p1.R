library(igraph)

# Construct an Igraph
nodes <- data.frame(names = c("A", "B", "C", "D", "E", "F"))
relations <- data.frame(
  from = c("B", "B", "C", "D", "D", "E", "F"), 
  to = c("C", "E", "A", "B", "E", "D", "C"))
g <- graph.data.frame(relations, directed = TRUE, vertices = nodes)

quartz()
plot(g)

# Run the page rank algorithm
pg <- page.rank(g, damping = 0.05)
round(pg$vector[sort.list(pg$vector, decreasing = TRUE,)],3)

pg <- page.rank(g, damping = 0.25)
round(pg$vector[sort.list(pg$vector, decreasing = TRUE,)],3)

pg <- page.rank(g, damping = 0.50)
round(pg$vector[sort.list(pg$vector, decreasing = TRUE,)],3)

pg <- page.rank(g, damping = 0.75)
round(pg$vector[sort.list(pg$vector, decreasing = TRUE,)],3)

pg <- page.rank(g, damping = 0.95)
round(pg$vector[sort.list(pg$vector, decreasing = TRUE,)],3)






