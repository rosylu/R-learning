#1.a Using hierarchical clustering with complete linkage and Euclidean distance, cluster the states. 
usarrest <- USArrests

usa.complete = hclust(dist(usarrest), method = "complete")
plot(usa.complete ,main = "US Arrest Complete Linkage ", xlab = "", sub = "", cex = .8)

#2.b Cut the dendrogram at a height that results in three distinct clusters. 
# Which states belong to which clusters? 

cut_usa <- cutree(usa.complete , 3)
# Cluster 1
names(cut_usa)[cut_usa == 1]
# Cluster 2
names(cut_usa)[cut_usa == 2]
# Cluster 3
names(cut_usa)[cut_usa == 3]


#2.c Hierarchically cluster the states using complete linkage and Euclidean distance,
# after scaling the variables to have standard deviation one. 

us_scale <- scale(usarrest)
us_scale.complete = hclust(dist(us_scale), method = "complete")
plot(us_scale.complete ,main = "Hierarchical Clustering with Scaled Features ", xlab = "", sub = "", cex = .8)

#(d) What effect does scaling the variables have on the hierarchical clustering obtained? 
# In your opinion, should the variables be scaled before the inter-observation dissimilarities are computed? 
# Provide a justification for your answer

# Cut the scaled dendrogram at a height that results in three distinct clusters
cut_us_scale <- cutree(us_scale.complete , 3)
# Cluster 1
names(cut_us_scale)[cut_us_scale == 1]
# Cluster 2
names(cut_us_scale)[cut_us_scale == 2]
# Cluster 3
names(cut_us_scale)[cut_us_scale == 3]

table(cut_usa, cut_us_scale)
