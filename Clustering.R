library(caret)
require(caret)
library(NbClust)
require(NbClust)
library(cluster)
require(cluster)
set.seed(1911)

raw_data <- read.csv('C:/Users/saipr/Desktop/Sai Priya/KDD/SharkAttackDataSets/SharkAttack_Balanced.csv')
raw_data$Attack..Y.N. <- ifelse(raw_data$Attack..Y.N. == 'Y', 1, 0)



clus_data <- subset(raw_data, select = c(DRYBULBTEMPF, WindSpeed, WindDirection,	SeaLevelPressure
                                         , Temp	, Sal ,	Turb, moonphase,	moonphasePlusTwo,
                                         moonphaseMinusTwo
))

clus_data$moonphase <- as.numeric(clus_data$moonphase)	
clus_data$moonphasePlusTwo <- as.numeric(clus_data$moonphasePlusTwo)
clus_data$moonphaseMinusTwo <- as.numeric(clus_data$moonphaseMinusTwo)

clus_data_dist <- dist(clus_data)




#Determining number of clusters
nbcl_Hc.out <- NbClust(clus_data, distance = "euclidean",
                       min.nc = 4, max.nc = 8, method = "complete", index = "silhouette")
nbcl_Hc.out$Best.nc

par(mfrow=c(1,1))


#Hierarchical clustering with default method = "complete"
complete.out <- hclust(clus_data_dist)
single.out <- hclust(clus_data_dist, method="single")
average.out <- hclust(clus_data_dist, method="average")


plot(complete.out, main='Dendrogram using complete linkage',cex=0.7)
plot(single.out, main='Dendrogram using single linkage',cex=0.7, hang=-1)
plot(average.out, main='Dendrogram using average linkage',cex=0.7, hang=-1)


#Playing with dendrogram objects
dndr_complete <- as.dendrogram(complete.out)
plot(dndr_complete, type="triangle")
plot(cut(dndr_complete, h = 4)$upper, main = "Upper tree of cut at h=4")
plot(cut(dndr_complete, h = 4)$lower[[4]], main = "Fourth branch of lower tree with cut at h=4") 


#Cut the dendrogram tree to create four clusters.
complete_c4.out<-cutree(complete.out,4)
table(complete_c4.out)
single_c4.out<-cutree(single.out,4)
table(single_c4.out)
average_c4.out<-cutree(average.out,4)
table(average_c4.out)
sapply(unique(complete_c4.out),function(g)rownames(clus_data)[complete_c4.out == g])


complete_sil<-silhouette(cutree(complete.out,4), clus_data_dist)
plot(complete_sil, main = "Silhouette plot for complete linkage result")
single_sil<-silhouette(cutree(single.out,4), clus_data_dist)
plot(single_sil, main = "Silhouette plot for single linkage result")
average_sil<-silhouette(cutree(average.out,4), clus_data_dist)
plot(average_sil, main = "Silhouette plot for Ward's method result")

#####################################################################################################

#kmeans

nbcl_Km.out <- NbClust(clus_data, distance = "euclidean",
                       min.nc = 4, max.nc = 8, method = "kmeans", index = "silhouette")
nbcl_Km.out$Best.nc
par(mfrow=c(1,1))

wssplot <- function(data, nc=15, seed=1234)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i, nstart=25)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

wssplot (clus_data)


# k-means algorithm
km.out <- kmeans(clus_data, 6, nstart = 25)
table(km.out$cluster)

# You may check the cluster membership and size of each cluster directly as follows
km.out
km.out$cluster
km.out$size



#clusplot
par(mfrow=c(1,2))
clusplot(clus_data_dist, diss = TRUE, km.out$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)


# Plots
par(mfrow=c(2,2))
plot(clus_data_dist,col=(km.out$cluster+1), main = "Clus Data")
par(mfrow=c(1,1))

#Silhouette
km_sil<-silhouette(km.out$cluster, clus_data_dist)
plot(km_sil, main = "Silhouette plot for k-means result")
summary(km_sil)
summary(km_sil)$avg.width

######################################################################################################


# members in each cluster for "hierarchical" clustering
countsHC <- sapply(2:4,function(ncl)table(cutree(complete.out, ncl)))
names(countsHC) <- 2:6
countsHC

# The following shows that kmeans clustering is not "hierarchical" at all.
countsKm <- sapply(2:4,function(ncl)table(kmeans(clus_data, ncl,nstart=25)$cluster))
names(countsKm) = 2:6
countsKm

# comparison of cluster membership
kmeans_com<-km.out$cluster
complete_com<-cutree(complete.out,6)
average_com<-cutree(average.out,6)
table (kmeans_com, complete_com)
table (kmeans_com, average_com)
table (average_com, complete_com)

###############################################################################################################

#Investigate model

raw_data$cluster <- complete_c4.out
with(raw_data, print(table(cluster, Attack..Y.N.)))
with(raw_data, print(by(Attack..Y.N., cluster, mean)))

raw_data2 <-raw_data
raw_data2$cluster <- km.out$cluster
with(raw_data2, print(table(cluster, Attack..Y.N.)))
with(raw_data2, print(by(Attack..Y.N., cluster, mean)))
