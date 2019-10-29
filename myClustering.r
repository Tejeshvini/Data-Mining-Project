library(readr)
library(cluster)
library(ggplot2)
library(tidyverse)
library(dendextend)
ilpd_df <- readRDS(file="ilpd_preprocessed.Rda")
head(ilpd_df)
#convert the dataframe to numeric
ilpd_df<-sapply(ilpd_df, as.numeric)
#scale the values in the df between 0 and 1
ilpd_scaled<-apply(ilpd_df, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
ilpd_scaled<-data.frame(ilpd_scaled)

##Clustering
#perform clustering after removing 1,2,10 and 11 columns with k=2 
ilpd_clustersk2<-kmeans(ilpd_scaled[,-c(1,2,10,11)], 2, nstart = 25)
#get the number of clusters
clusters_numk2 <- as.factor(ilpd_clustersk2$cluster)
#plot the clusters for k=2
ggplot(ilpd_scaled, aes(Alkphos, TP, color = clusters_numk2)) + geom_point()
#plot according to class names
ggplot(ilpd_scaled, aes(Alkphos, TP, color = ilpd_scaled$Class)) + geom_point()
#clustering for k=3
ilpd_clustersk3<-kmeans(ilpd_scaled[,-c(1,2,10,11)], 3, nstart = 25)
clusters_numk3 <- as.factor(ilpd_clustersk3$cluster)
ggplot(ilpd_scaled, aes(Alkphos, TP, color = clusters_numk3)) + geom_point()
#clustering for k=4
ilpd_clustersk4<-kmeans(ilpd_scaled[,-c(1,2,10,11)], 4, nstart = 25)
clusters_numk4 <- as.factor(ilpd_clustersk4$cluster)
ggplot(ilpd_scaled, aes(Alkphos, TP, color = clusters_numk4)) + geom_point()
#clustering for k=5
ilpd_clustersk5<-kmeans(ilpd_scaled[,-c(1,2,10,11)], 5, nstart = 25)
clusters_numk5 <- as.factor(ilpd_clustersk5$cluster)
ggplot(ilpd_scaled, aes(Alkphos, TP, color = clusters_numk5)) + geom_point()

##SSE
wss = kmeans(ilpd_scaled[,-c(1,2,10,11)], centers=1)$tot.withinss
for (i in 1:5)
  wss[i] = kmeans(ilpd_scaled[,-c(1,2,10,11)], i)$tot.withinss
wss
sse = data.frame("Clusters"=c(1:5), "SSE"=c(wss))
png(filename = "SSE.png",width = 900, height = 600)
plot(sse$SSE,type="o",col="red",xlab ="Clusters",ylab="SSE",main="SSE")
dev.off()

##Hierarchical clustering
#create a samller sample of data from the main data for hierarchical clustering
set.seed(45)
ilpd_sample <- sample(2, nrow(ilpd_scaled),replace = TRUE, prob = c(0.2, 0.8))
sample_data<-ilpd_df[ilpd_sample==1,]
#create a distance matrix sing euclidean distance
d<-dist(sample_data,method="euclidean")
#hclust is used to create a dendogram
hclust_default<-hclust(d)
dend<- as.dendrogram(hclust_default)
#Color the dendigram into 2 clusters
dend_2<- color_branches(dend,k=2)
png(filename = "dend_2.png",width = 900, height = 600)
plot(dend_2,cex = 0.6, hang = -1, main= "Dendogram with 2 Clusters")
dev.off()
#dendogram with 3 clusters
dend_3<- color_branches(dend,k=3)
png(filename = "dend_3.png",width = 900, height = 600)
plot(dend_3,cex = 0.6, hang = -1,main= "Dendogram with 3 Clusters")
dev.off()
#dendogram with 4 clusters
dend_4<- color_branches(dend,k=4)
png(filename = "dend_4.png",width = 900, height = 600)
plot(dend_4,cex = 0.6, hang = -1, main= "Dendogram with 4 Clusters")
dev.off()
#dendogram with 5 clusters
dend_5<- color_branches(dend,k=5)
png(filename = "dend_5.png",width = 900, height = 600)
plot(dend_5,cex = 0.6, hang = -1, main= "Dendogram with 5 Clusters")
dev.off()

##Heirarchical Clustering wit agglomerationmethods
#Average method
hclust_avg<-hclust(d, method = "avg")
dend<- as.dendrogram(hclust_avg)
dend_avg<- color_branches(dend,k=2)
png(filename = "dend_avg.png",width = 900, height = 600)
plot(dend_avg,cex = 0.6, hang = -1, main= "Dendogram with average method")
dev.off()
#Maximum method
hclust_com<-hclust(d, method = "com")
dend_com<- as.dendrogram(hclust_com)
dend_com<- color_branches(dend_com,k=2)
png(filename = "dend_com.png",width = 900, height = 600)
plot(dend_avg,cex = 0.6, hang = -1, main= "Dendogram with maximum method")
dev.off()
#Minimum method
hclust_sin<-hclust(d, method = "sin")
dend_sin<- as.dendrogram(hclust_sin)
dend_sin<- color_branches(dend_sin,k=2)
png(filename = "dend_sin.png",width = 900, height = 600)
plot(dend_avg,cex = 0.6, hang = -1, main= "Dendogram with minimum method")
dev.off()



