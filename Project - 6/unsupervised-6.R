#Agglomerative Clustering

safety <- read.csv(file.choose(), header = TRUE, sep = ",")
safety <- safety[2:8]
str(safety)

library(cluster)
clusters_safety <- agnes(x=safety, diss = FALSE, stand = TRUE, method = "average")
DendCluster_safety <- as.dendrogram(clusters_safety)
plot(DendCluster_safety)

clusters_safety <- agnes(x=safety, diss = FALSE, stand = TRUE, method = "complete")
DendCluster_safety <- as.dendrogram(clusters_safety)
plot(DendCluster_safety)

clusters_safety <- agnes(x=safety, diss = FALSE, stand = TRUE, method = "single")
DendCluster_safety <- as.dendrogram(clusters_safety)
plot(DendCluster_safety)

#Divisive clustering

fit_safety <- diana(safety[,], metric = "manhattan", stand = TRUE)
plot(fit_safety)


