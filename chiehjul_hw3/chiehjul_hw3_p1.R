rm(list = ls())

#install.packages('kohonen')
library(kohonen)
require(graphics)


# load nci data
load("~/Documents/DS/EAS 507/HW3/ElemStatLearn/data/nci.RData")
nci.measure <- unique(colnames(nci))
nci.scale <-scale(t(nci))

# 14 subtypes of tumor cells
#unique(colnames(nci))

# Fit an SOM
set.seed(123)
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
nci.som <- som(nci.scale, grid = som_grid, rlen = 5000)


codes <- nci.som$codes[[1]]
#?plot.kohonen

plot(nci.som, main = "nci Data")
plot(nci.som, type = "changes", main = "nci Data with rlen = 5000")
#Empty nodes indicate that your map size is too big for the number of samples
plot(nci.som, type = "count")

plot(nci.som, type = "mapping")

#U-matrix
#Areas with large distances indicate the nodes are much more dissimilar.
coolBlueHotRed <- function(n, alpha = 1){rainbow(n, end=4/6, alpha = alpha)[n:1]}
plot(nci.som, type = "dist.neighbours", palette.name = coolBlueHotRed)

# component plane plots
for (i in 1:25){
  plot(nci.som, type = "property", property=codes[,i], main = colnames(codes)[i])
}

d <- dist(codes)
hc <- hclust(d)

plot(hc)
rect.hclust(hc, k = 4, border = 3:5)


som_cluster <- cutree(hc, h = 120)

# plot the SOM with the found clusters
my_pal <- c("red", "blue", "yellow", "orange")
my_bhcol <- my_pal[som_cluster]

plot(nci.som, type = "mapping", col = "black", bgcol = my_bhcol )
add.cluster.boundaries(nci.som, som_cluster)

cluster_result <- som_cluster[nci.som$unit.classif]

# Print cluster info
for (i in 1:4){
  each_cluster <- gsub('V', '', names(som_cluster)[som_cluster == i])
  cat('Cluster', i, "\n")
  for (clu in each_cluster) {
    cat(rownames(nci.scale)[nci.som$unit.classif == clu], ' ')
  }
  cat(  fill = TRUE)
}

nodes <- data.frame("name" = rownames(nci.scale), "node" = nci.som$unit.classif)