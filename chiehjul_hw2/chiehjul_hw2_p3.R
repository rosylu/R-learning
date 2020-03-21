#3.a Cluster the data based on single-linkage, average linkage, and complete-linkage agglomerative hierarchical clustering. 
# Do not use the “seed group” column to perform the clustering, but use it to help evaluate your results.
#install.packages("cluster")
#install.packages("fpc")
library("cluster")
library("fpc")

seeds <- read.delim("Documents/DS/Data mining/HW2/seeds_dataset.txt")
seed.select <- seeds[ ,1:7]

seed.scale <- scale(seed.select)
seed.dis <- dist(seed.scale)

# dendrogram
seed.sing = hclust(seed.dis, method = "single")
plot(seed.sing, main = " Single Linkage with Correlation-Based Distance ", xlab = "", sub = "", cex = .8)
seed.avg = hclust(seed.dis, method = "average")
plot(seed.avg, main = "Average Linkage with Correlation-Based Distance ", xlab = "", sub = "", cex = .8)
seed.comp = hclust(seed.dis, method = "complete")
plot(seed.comp, main = " Complete Linkage with Correlation-Based Distance ", xlab = "", sub = "", cex = .8)

# Use silhouette and get the best average silhouette width

sing.avg_width <- c()
for (i in 2:6){
  ct <- cutree(seed.sing, k=i)
  si <- silhouette(ct, dist = seed.dis)
  avg_width <- summary(si)$avg.width
  sing.avg_width <- c(sing.avg_width, avg_width)
}
sing.avg_width

# silhouette plot
sing.ct <- cutree(seed.sing, k = 2)
sing.si <- silhouette(sing.ct, dist = seed.dis )

plot(sing.si)

avg.avg_width <- c()
for (i in 2:6){
  ct <- cutree(seed.avg, k=i)
  si <- silhouette(ct, dist = seed.dis)
  avg_width <- summary(si)$avg.width
  avg.avg_width <- c(avg.avg_width, avg_width)
}
avg.avg_width

# silhouette plot

avg.ct <- cutree(seed.avg, k = 2)
avg.si <- silhouette(avg.ct, dist = seed.dis )

plot(avg.si)

comp.avg_width <- c()
for (i in 2:6){
  ct <- cutree(seed.comp, k=i)
  si <- silhouette(ct, dist = seed.dis)
  avg_width <- summary(si)$avg.width
  comp.avg_width <- c(comp.avg_width, avg_width)
}
comp.avg_width

# silhouette plot
comp.ct <- cutree(seed.comp, k = 2)
comp.si <- silhouette(comp.ct, dist = seed.dis )

plot(comp.si)

# Accuracy
sing_table <- table(seeds[,8], sing.ct)
sing_accurate <- sum(diag(sing_table))/sum(sing_table)
sing_accurate

avg_table <- table(seeds[,8], avg.ct)
avg_accurate <- sum(diag(avg_table))/sum(avg_table)
avg_accurate

com_table <- table(seeds[,8], comp.ct)
com_accurate <- sum(diag(com_table))/sum(com_table)
com_accurate


# 3.b Cluster the data based on K-means or K-medoids.
gap_kmed <- clusGap(seed.select, pam, K.max = 10, B = 100)

plot(gap_kmed, main = "Gap Statistic: kmedoids")

## k-mediods
kmed <- pamk(seed.select, 3)

# tabulate the results 
table(kmed$pamobject$clustering, seeds$Seed.Group)

# Calculate accuracy
kmed_table <- table(kmed$pamobject$clustering, seeds$Seed.Group)
kmed_accurate <- sum(diag(kmed_table))/sum(kmed_table)
kmed_accurate

# plot the results for k= 3
layout(matrix(c(1,2), 1, 2))
plot(kmed$pamobject)

## k-mediods
kmed_2 <- pamk(seed.select, 2)

# tabulate the results 
table(kmed_2$pamobject$clustering, seeds$Seed.Group)

# Calculate accuracy
kmed2_table <- table(kmed_2$pamobject$clustering, seeds$Seed.Group)
kmed2_accurate <- sum(diag(kmed2_table))/sum(kmed2_table)
kmed2_accurate

# plot the results for k= 2
layout(matrix(c(1,2), 1, 2))
plot(kmed_2$pamobject)

