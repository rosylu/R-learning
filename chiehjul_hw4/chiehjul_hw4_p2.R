library(Rgraphviz)
library(gRbase)
library(gRain)
library(ggm)
#load("~/Documents/DS/EAS 507/HW 4/gRbase/data/cad1.RData")
#data(cad1)

## Specify the DAG
g <- list(~A, ~C|A, ~B, ~D|A:B, ~E|B, ~F|A:C:E, ~G|D:E, ~H|F:G)
chestdag <- dagList(g)

# Check seperate
dSep(as(chestdag, "matrix"), "C", "G", c())
dSep(as(chestdag, "matrix"), "C", "E", c())

# Check d-connected
!dSep(as(chestdag, "matrix"), "C", "E", "G")
!dSep(as(chestdag, "matrix"), "A", "G", c("D","E"))
!dSep(as(chestdag, "matrix"), "A", "G", "D")
