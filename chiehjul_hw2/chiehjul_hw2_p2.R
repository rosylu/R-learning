#install.packages("tidyverse")
#install.packages("rlang")
#install.packages("ggplot2", dependencies=TRUE)
library("ggplot")

#2.a) Load in the data using read.csv(). You will need to select header=F. 

gene <- read.csv("Ch10Ex11.csv", header = FALSE)

#2.b) Apply hierarchical clustering to the samples using correlationbased distance, 
# and plot the dendrogram. Do the genes separate the samples into the two groups? 
# Do your results depend on the type of linkage used? 

## Generate plot for correlation-based dis
core_dis <- as.dist(1-cor(gene))
plot(hclust(core_dis, method = "complete" ), main = " Complete Linkage with Correlation-Based Distance ", xlab = "", sub = "", cex = .8)
plot(hclust(core_dis, method = "single" ), main = " Single Linkage with Correlation-Based Distance ", xlab = "", sub = "", cex = .8)
plot(hclust(core_dis, method = "average" ), main = " Average Linkage with Correlation-Based Distance ", xlab = "", sub = "", cex = .8)

## 2 clusters in each type of linkage used
cut_comp <- cutree(hclust(core_dis, method = "complete" ) , 2)
## Cluster 1
names(cut_comp)[cut_comp == 1]
## Cluster 2
names(cut_comp)[cut_comp == 2]

cut_sing <- cutree(hclust(core_dis, method = "single" ) , 2)
## Cluster 1
names(cut_sing)[cut_sing == 1]
## Cluster 2
names(cut_sing)[cut_sing == 2]

cut_avg <- cutree(hclust(core_dis, method = "average" ) , 2)
## Cluster 1
names(cut_avg)[cut_avg == 1]
## Cluster 2
names(cut_avg)[cut_avg == 2]


#2.c) Your collaborator wants to know which genes differ the most across the two groups. 
# Suggest a way to answer this question, and apply it here.

fit = prcomp(gene, center=TRUE,scale=TRUE)

biplot(fit, main = "PCA plot")

PC1 <- fit$x[,1]
PC2 <- fit$x[,2]
PC_dats <- cbind(PC1, PC2)
km2 <- kmeans(PC_dats, centers = 2)

# plot and point the centers
plot(PC_dats, col = km2$cluster, main = "K-means w/PC")
points(km2$centers, col = 1:3, pch = 8, cex= 2)
