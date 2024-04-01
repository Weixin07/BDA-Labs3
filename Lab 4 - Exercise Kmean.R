library(rattle)

data(wine, package="rattle")
head(wine)

df <- scale(wine[-1])                                         #1

#wssplot(df)                                                #2
#WSS Method
#to find best K for the cluster (2 & 3 are the elbow points, test both of em and decide, best if not overlapping)

tot.withinss <- vector(mode="character", length=10)
for (i in 1:10){
  wineCluster <- kmeans(df[,1:4], center=i, nstart=20)
  tot.withinss[i] <- wineCluster$tot.withinss
}


plot(1:10, tot.withinss, type="b", pch=19)



library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.n[1,])



barplot(table(nc$Best.n[1,]),
          xlab="Numer of Clusters", ylab="Number of Criteria",
          main="Number of Clusters Chosen by 26 Criteria")

#wss equation to decide on K value

# The KMean
set.seed(1234)
fit.km <- kmeans(df, 3, nstart=25)                           #3
fit.km$size


aggregate(wine[-1], by=list(cluster=fit.km$cluster), mean)

#scatterplot
ggplot(df, aes(Petal.Length, Petal.Width)) + geom_point(aes(col=Species), size=4)

set.seed(25)
irisCluster <- kmeans(df[,1:2], center=4, nstart=20)
irisCluster
clusplot(iris, irisCluster$cluster, color=T, shade=T, labels=0, lines=0)

#1 standardize data
#2 determine number of clusters
#3 K-means cluster analysis

