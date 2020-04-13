#2.a Perform hierarchical clustering with complete linkage and Euclidean distance to cluster the states. 
rm(list = ls())

require(graphics)

usarrest <- scale(USArrests)

usa.complete = hclust(dist(usarrest), method = "complete")

plot(usa.complete ,main = "US Arrest Complete Linkage ", xlab = "", sub = "", cex = .8)
rect.hclust(usa.complete, k = 4, border = 3:5)

#Cut the dendrogram at a height that results in three clusters.  

cut_usa <- cutree(usa.complete , 4)

#2.b Fit a SOM to the data and present the results
#install.packages('kohonen')
library(kohonen)

usarrest.scale <-scale(usarrest)

set.seed(123)
som_grid <- somgrid(xdim = 4, ydim = 4, topo = "hexagonal")
usarrest.som <- som(usarrest.scale, grid = som_grid, rlen = 4000)

codes <- usarrest.som$codes[[1]]

plot(usarrest.som, main = "usarrest Data")
plot(usarrest.som, type = "changes", main = "usarrest Data with rlen = 4000")
#Empty nodes indicate that your map size is too big for the number of samples
plot(usarrest.som, type = "count")
plot(usarrest.som, type = "mapping")

#U-matrix
#Areas with large distances indicate the nodes are much more dissimilar.
coolBlueHotRed <- function(n, alpha = 1){rainbow(n, end=4/6, alpha = alpha)[n:1]}
plot(usarrest.som, type = "dist.neighbours", palette.name = coolBlueHotRed)

# component plane plots
for (i in 1:4){
  quartz()
  plot(usarrest.som, type = "property", property=codes[,i], main = colnames(codes)[i])
}

# Dendrogram and cut the tree
d <- dist(codes)
hc <- hclust(d)
plot(hc)
rect.hclust(hc, k = 4, border = 3:5)

som_cluster <- cutree(hc, h = 3)

# plot the SOM with the found clusters

my_pal <- c("red", "blue", "yellow", "orange")
my_bhcol <- my_pal[som_cluster]

plot(usarrest.som, type = "mapping", col = "black", bgcol = my_bhcol, cex = 0.9, labels=(rownames(usarrest)))
add.cluster.boundaries(usarrest.som, som_cluster)


# Present each cluster 
## hierarchical clustering
for (i in 1:4){ 
  cat('Cluster', i, "\n")
  cat(names(cut_usa)[cut_usa == i], "\n")
}

## SOM
for (i in 1:4){
  each_cluster <- gsub('V', '', names(som_cluster)[som_cluster == i])
  cat('Cluster', i, "\n")
  for (clu in each_cluster) {
    cat(rownames(usarrest)[usarrest.som$unit.classif == clu], ' ')
  }
  cat(  fill = TRUE)
}