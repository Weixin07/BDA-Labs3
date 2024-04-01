library(ggplot2)
library(cluster)
df <- iris
head(iris)

#scatterplot
ggplot(df, aes(Petal.Length, Petal.Width)) + geom_point(aes(col=Species), size=4)

set.seed(25)
irisCluster <- kmeans(df[,1:2], center=4, nstart=20)
irisCluster
clusplot(iris, irisCluster$cluster, color=T, shade=T, labels=0, lines=0)

#WSS Method
#to find best K for the cluster (2 & 3 are the elbow points, test both of em and decide, best if not overlapping)

tot.withinss <- vector(mode="character", length=10)
for (i in 1:10){
  irisCluster <- kmeans(df[,1:4], center=i, nstart=20)
  tot.withinss[i] <- irisCluster$tot.withinss
}


plot(1:10, tot.withinss, type="b", pch=19)

