

rm(list = ls())
library(tidyverse)
library(cluster)
library(factoextra)
library(gridExtra)
library(dplyr)

Path<-setwd("D:/R assignments/Case Study 3")
customers<-read.csv("Mall_Customers.csv", header = TRUE, as.is = TRUE, na.strings = c(""))


str(customers)
summary(customers)
as.data.frame(colnames(customers))

custdata= select(customers, c("Age","Annual.Income..k..","Spending.Score..1.100."))

as.data.frame(colSums(is.na(custdata)))



library(caret)
preproc<-preProcess(custdata)
custdataNorm<-predict(preproc,custdata)
summary(custdataNorm)


e_distance<-dist(custdataNorm, method = "euclidean")
Clustercustomers<-hclust(e_distance, method = "ward.D")
plot(Clustercustomers)


Clustercustomers<-cutree(Clustercustomers, k = 6)
table(Clustercustomers)


Clustercustomers1<-cutree(Clustercustomers, k = 5)
table(Clustercustomers1)

Clustercustomers2<-cutree(Clustercustomers, k = 4)
table(Clustercustomers2)


MeanComp<-function(var, clustergrp, meas){
  z<-tapply(var, clustergrp, meas)
  print(z)
}

Score_mean<-MeanComp(custdata$Spending.Score..1.100., Clustercustomers, mean)
Score_mean<-MeanComp(custdata$Spending.Score..1.100., Clustercustomers1, mean)
Score_mean<-MeanComp(custdata$Annual.Income..k.., Clustercustomers1, mean)


Customers_H<-data.frame(customers,Clustercustomers1)
write.csv(Customers_H,"Customers_Hierarchical.csv", row.names = FALSE)


set.seed(22)

CustCluster_K1<-kmeans(custdataNorm, centers = 4,iter.max = 100)
CustCluster_K2<-kmeans(custdataNorm, centers = 5,iter.max = 100)
CustCluster_K3<-kmeans(custdataNorm, centers = 6,iter.max = 100)
CustCluster_K4<-kmeans(custdataNorm, centers = 7,iter.max = 100)


plot1 <- fviz_cluster(CustCluster_K1, geom = "point", data = custdataNorm) + ggtitle("k = 4")
plot2 <- fviz_cluster(CustCluster_K2, geom = "point",  data = custdataNorm) + ggtitle("k = 5")
plot3 <- fviz_cluster(CustCluster_K3, geom = "point",  data = custdataNorm) + ggtitle("k = 6")
plot4 <- fviz_cluster(CustCluster_K4, geom = "point",  data = custdataNorm) + ggtitle("k = 7")

grid.arrange(plot1, plot2, plot3, plot4, nrow = 2)


set.seed(123)
fviz_nbclust(custdataNorm, kmeans, method = "wss")

k<-4
CustCluster_K<-kmeans(custdataNorm, centers = k,iter.max = 1000)
CustCluster_K



table(CustCluster_K$cluster)
CustCluster_K$centers
fviz_cluster(CustCluster_K, data = custdataNorm)



Spend_mean_k<-aggregate(custdata, by=list(cluster=CustCluster_K$cluster), mean)
Spend_mean<-MeanComp(custdata$Spending.Score..1.100., CustCluster_K$cluster, mean)


Customers_k <- data.frame(customers, CustCluster_K$cluster)
write.csv(Customers_k,"Customers_k-Means.csv", row.names = FALSE)
write.csv(Spend_mean_k,"Customers_k-Means_Summary.csv", row.names = FALSE)

