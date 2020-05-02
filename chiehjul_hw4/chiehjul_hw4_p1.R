# 1.a Focus on the karate network.

#install.packages("“igraphdata")
#install.packages("“igraph")
library(igraph)
library(igraphdata)

data(karate)
data(kite)


karate.lay <- layout_nicely(karate)

plot(karate, layout = karate.lay)

# Delete edges, create noisy datasets
edgelist <- get.edgelist(karate)
num_of_edge <- nrow(get.edgelist(karate))

## 1.a. Random delete 5% edge
del_edge <- sample(nrow(edgelist), round(num_of_edge*0.05))
edgelist[del_edge,]
noisy_karate <- delete_edges(karate, t(del_edge))

plot(noisy_karate, layout = karate.lay)

E(noisy_karate)$color <- "gray"
E(noisy_karate)$width <- 1
ori_karate <- add_edges(noisy_karate, t(edgelist[del_edge,]), color = "red", width=3)
# Plot the deleted edge on the table
plot(ori_karate, layout = karate.lay)

## Fit a HRG model to the network

hrg <- fit_hrg(noisy_karate)
plot_dendrogram(hrg)

hrg 

# The fitted model, more details
#print(hrg, level=5)

# Predict
pred <- predict_edges(noisy_karate)
pred

plot(pred$prob)
pred$edges[1:length(del_edge),]

pred_karate <- add_edges(noisy_karate, t(pred$edges[1:length(del_edge),]), color = "orange", width = 3)
plot(pred_karate, layout = karate.lay)


# 1.b 
data(kite)

kite.lay <- layout_nicely(kite)

plot(kite, layout = kite.lay)

# Delete edges, create noisy datasets
edgelist <- get.edgelist(kite)
num_of_edge <- nrow(get.edgelist(kite))

## 1.a. Random delect 5% edge
del_edge <- sample(nrow(edgelist), round(num_of_edge*0.05))
edgelist[del_edge,]
noisy_kite <- delete_edges(kite, t(del_edge))

plot(noisy_kite, layout = kite.lay)

E(noisy_kite)$color <- "gray"
E(noisy_kite)$width <- 1
ori_kite <- add_edges(noisy_kite, t(edgelist[del_edge,]), color = "red", width=3)
# Plot the deleted edge on the table
plot(ori_kite, layout = kite.lay)

## Fit a HRG model to the network

hrg <- fit_hrg(noisy_kite)
hrg

# Predict
pred <- predict_edges(noisy_kite)
pred

plot(pred$prob)
pred$edges[1:length(del_edge),]

pred_kite <- add_edges(noisy_kite, t(pred$edges[1:length(del_edge),]), color = "orange", width = 3)
plot(pred_kite, layout = kite.lay)

## 1.c. Random delete 15% and 40% edges

for (del_num in c(0.15,0.4))
{
  #########
  # karate
  #########
  plot(karate, layout = karate.lay)
  
  # Delete edges, create noisy datasets
  edgelist <- get.edgelist(karate)
  num_of_edge <- nrow(get.edgelist(karate))
  
  del_edge <- sample(nrow(edgelist), round(num_of_edge*del_num))
  edgelist[del_edge,]
  noisy_karate <- delete_edges(karate, t(del_edge))

  plot(noisy_karate, layout = karate.lay)

  E(noisy_karate)$color <- "gray"
  E(noisy_karate)$width <- 1
  ori_karate <- add_edges(noisy_karate, t(edgelist[del_edge,]), color = "red", width=3)
  # Plot the deleted edge on the table
  plot(ori_karate, layout = karate.lay)

  ## Fit a HRG model to the network
  hrg <- fit_hrg(noisy_karate)
  #plot_dendrogram(hrg)
  hrg 

  # Predict
  pred <- predict_edges(noisy_karate)
  pred$edges[1:length(del_edge),]

  pred_karate <- add_edges(noisy_karate, t(pred$edges[1:length(del_edge),]), color = "orange", width = 3)
  plot(pred_karate, layout = karate.lay)
  
  ########
  ## kite
  ########
  plot(kite, layout = kite.lay)
  
  # Delete edges, create noisy datasets
  edgelist <- get.edgelist(kite)
  num_of_edge <- nrow(get.edgelist(kite))
  
  del_edge <- sample(nrow(edgelist), round(num_of_edge*del_num))
  edgelist[del_edge,]
  noisy_kite <- delete_edges(kite, t(del_edge))
  
  plot(noisy_kite, layout = kite.lay)
  
  E(noisy_kite)$color <- "gray"
  E(noisy_kite)$width <- 1
  ori_kite <- add_edges(noisy_kite, t(edgelist[del_edge,]), color = "red", width=3)
  # Plot the deleted edge on the table
  plot(ori_kite, layout = kite.lay)
  
  ## Fit a HRG model to the network
  
  hrg <- fit_hrg(noisy_kite)
  hrg
  
  # Predict
  pred <- predict_edges(noisy_kite)
  pred
  
  plot(pred$prob)
  pred$edges[1:length(del_edge),]
  
  pred_kite <- add_edges(noisy_kite, t(pred$edges[1:length(del_edge),]), color = "orange", width = 3)
  plot(pred_kite, layout = kite.lay)
  
}