library(ISLR)
library(tidyverse)

RNGkind(sample.kind = "Rounding")
set.seed(1)

# Q1 ----------------------------------------------------------------------


nci.labs <- NCI60$labs
nci.data <- NCI60$data
sd.data <- scale(nci.data)

par(mfrow = c(3, 1))
data.dist <- dist(sd.data)

plot(hclust(data.dist), 
     labels = nci.labs, 
     main = "Complete Linkage", 
     xlab = "", 
     ylab = "", 
     sub = "")

plot(hclust(data.dist, method = "average"), 
     labels = nci.labs, 
     main = "Average Linkage", 
     xlab = "", 
     ylab = "", 
     sub = "")

plot(hclust(data.dist, method = "single"), 
     labels = nci.labs, 
     main = "Single Linkage", 
     xlab = "", 
     ylab = "", 
     sub = "")

hc.out <- hclust(data.dist)
hc.clusters <- cutree(hc.out, 4)
table(hc.clusters, nci.labs)

par(mfrow = c(1, 1))
plot(hc.out, labels = nci.labs)
abline(h = 139, col = "red")

# 5 clusters
hc.clusters <- cutree(hc.out, 5)
table(hc.clusters, nci.labs)

# Q2 ----------------------------------------------------------------------

set.seed(2)
km.out = kmeans(sd.data, 4, nstart=20)
km.clusters = km.out$cluster
table(km.clusters, hc.clusters)

table(km.clusters,nci.labs)


# Q3 ----------------------------------------------------------------------

set.seed(2)
km.ns.out = kmeans(nci.data, 4, nstart=20)
km.ns.clusters = km.ns.out$cluster
table(km.clusters, km.ns.clusters)

table(km.clusters,nci.labs)
table(km.ns.clusters,nci.labs)
