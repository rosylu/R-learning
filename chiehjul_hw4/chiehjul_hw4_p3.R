#install.packages("BiocManager")
#BiocManager::install("Rgraphviz")

#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("Rgraphviz")
#BiocManager::install("gRbase")
#install.packages("ggm")
#install.packages("gRain")

library(Rgraphviz)
library(gRbase)
library(gRain)
library(ggm)
#library(ggplot2)
#library(ggdag)


data(cad1)

## Specify the DAG
g <- list(~sex, ~smoker|sex, ~suffheartf, ~hyperchol|suffheartf:smoker, ~inherit|smoker, ~cad|inherit:hyperchol)
chestdag <- dagList(g)

## Inquire about d-separation
dSep(as(chestdag, "matrix"), "suffheartf", "smoker", c())
dSep(as(chestdag, "matrix"), "suffheartf", "smoker", c("inherit", "cad"))
dSep(as(chestdag, "matrix"), "suffheartf", "smoker", "cad")
dSep(as(chestdag, "matrix"), "sex", "hyperchol", c("smoker"))

# 1.a Construct this network in R, and infer the Conditional Probability Tables using the cad1 data
#     Identify any d-separations in the graph.

## Specify the CPD tables

fm <- c("Female", "Male")
m_per <- nrow(cad1[(cad1$Sex == "Male"),])/nrow(cad1)
f_per <- nrow(cad1[(cad1$Sex == "Female"),])/nrow(cad1)

cpd_sex <- cptable(  ~sex, values = c(f_per,m_per), levels =fm)

yn <- c("yes", "no")

cpd_smoker.sex <- cptable(~smoker|sex, values = c(
  nrow(cad1[( cad1$Sex == "Female" & cad1$Smoker == "Yes" ),])/nrow(cad1[(cad1$Smoker == "Yes"),]),
  nrow(cad1[( cad1$Sex == "Female" & cad1$Smoker == "No" ),])/nrow(cad1[(cad1$Smoker == "No"),]),
  nrow(cad1[( cad1$Sex == "Male" & cad1$Smoker == "Yes" ),])/nrow(cad1[(cad1$Smoker == "Yes"),]),
  nrow(cad1[( cad1$Sex == "Male" & cad1$Smoker == "No" ),])/nrow(cad1[(cad1$Smoker == "No"),])
  ), levels =yn)

cpd_suffheartf <- cptable(~suffheartf, values = c(
  nrow(cad1[(cad1$SuffHeartF == "Yes"),])/nrow(cad1),
  nrow(cad1[(cad1$SuffHeartF == "No"),])/nrow(cad1)
  ), levels =yn)

cpd_hyperchol.suffheartf.smoker <- cptable(~hyperchol|suffheartf:smoker,
                                           values = c(cad1$Hyperchol,cad1$SuffHeartF,cad1$Smoker), levels =yn)
cpd_inherit.smoker<- cptable(~inherit|smoker, values = c(cad1$Inherit,cad1$Smoker), levels =yn)
cpd_cad.inherit.hyperchol<- cptable(~cad|inherit:hyperchol,
                                    values = c(cad1$CAD,cad1$Inherit,cad1$Hyperchol), levels =yn)


dfg <- ~Sex + Smoker*Sex+ SuffHeartF+ Hyperchol*SuffHeartF*Smoker+ Inherit*Smoker+ CAD*Inherit*Hyperchol
dg    <- dag(dfg)


ecpt <- extractCPT(cad1[c(1,8,10,11,12,14)],dg)
## Build the network
plist <- compileCPT(ecpt)

grn1 <- grain(plist)
summary(grn1)

plot(grn1$dag)

## Compile the network 
## DAG is created, moralized, and triangularized.

grn1c <- compile(grn1)
summary(grn1c)

# if interested in "haulting" the compilation process
g <- grn1$dag
mg <- moralize(g)
tmg <- triangulate(mg)
rip(tmg)

# plot the junction tree

plot(grn1c$dag)

# 3.b
## Propagate the the network 

grn1c <- propagate(grn1c)

##################################################
## Make Queries
## 
##################################################
# Suppose we have some information, we want to "absorb it"
grn1c.ev <- setFinding(grn1c, nodes = c("Sex","Hyperchol"), states = c("Female","Yes"))

# probabilistic query, given evidence
abs <- querygrain(grn1c.ev, nodes = c("SuffHeartF", "CAD"), type = "marginal")
not_abs <- querygrain(grn1c, nodes = c("SuffHeartF", "CAD"), type = "marginal")


# Calculate the probabilty of observing evidence
getFinding(grn1c.ev)

# probabilistic queries, given evidence, joint distribution
a <- querygrain(grn1c.ev, nodes = c("SuffHeartF"))

# probabilistic queries, given evidence, conditional distribution
querygrain(grn1c.ev, nodes = c("CAD", "Hyperchol"), type = "conditional")

