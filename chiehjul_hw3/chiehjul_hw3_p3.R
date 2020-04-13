
rm(list = ls())

#install.packages('kohonen')
#install.packages('ggplot2')
#install.packages('factoextra')
#install.packages('ggfortify')

library(kohonen)
library(factoextra)
library(ggfortify)


#load("~/Documents/DS/EAS 507/HW3/SwissBankNotes.rdata")
swissbank <- SwissBankNotes
genuine <- swissbank[1:100,]
counterfeit <- swissbank[101:200,]

# principal components

pc <- princomp(swissbank)
swissbank_scale <- prcomp(swissbank, center = TRUE, scale = TRUE)
genuine_scale <- prcomp(genuine, center = TRUE, scale = TRUE)
counterfeit_scale <- prcomp(counterfeit, center = TRUE, scale = TRUE)

# Scree plot
fviz_eig(pc, main = 'All scree plot (unscaled)')

fviz_eig(swissbank_scale, main = 'All scree plot (scaled)')
fviz_eig(genuine_scale, main = 'Genuine scree plot')
fviz_eig(counterfeit_scale, main = 'Counterfeit scree plot')

summary(swissbank_scale)
summary(genuine_scale)
summary(counterfeit_scale)

# Add group col
swissbank$group <- 'Counterfeit'
swissbank[1:100,]$group <- 'Genuine'

# Score plot

fviz_pca_ind(swissbank_scale, geom.ind = "point", habillage = swissbank$group, palette = c("#00AFBB", "#FC4E07"),
             addEllipses = TRUE, legend.title = "Groups", title = "Total score plot")

fviz_pca_ind(genuine_scale, col.ind = "#FC4E07",title = "Genuine score plot")
fviz_pca_ind(counterfeit_scale, col.ind = "#00AFBB",  title = "Counterfeit score plot")

fviz_pca_biplot(genuine_scale, label="var",col.ind = "#FC4E07",title = "Genuine diagnostic plot")
fviz_pca_biplot(counterfeit_scale, label="var", col.ind = "#00AFBB",  title = "Counterfeit diagnostic plot")
fviz_pca_biplot(swissbank_scale, label="var", , habillage = swissbank$group, palette = c("#00AFBB", "#FC4E07"),
                addEllipses = TRUE, title = "Total diagnostic plot")

biplot(swissbank_scale)
