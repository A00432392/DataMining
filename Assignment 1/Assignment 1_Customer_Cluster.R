library(ggplot2)
library(GGally)
library(DMwR)
library(factoextra) # clustering algorithms & visualization
library(NbClust)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms

set.seed(5580)

#Original Data

cust = read.csv('customercluster.csv')
View(cust)

#Plot scatters plot.
ggpairs(cust[,which(names(cust)!="CustomerID")], upper = list(continuous = ggally_points),
        lower = list(continuous = "points"), title = "Customers Before Outlier Removal")

#Clean Data - Remove Outliers
cust.clean <- cust[cust$CustomerID != "0", ]
cust.clean <- cust.clean[cust.clean$CustomerID != "14646", ]

#Plot scatters plot.
ggpairs(cust.clean[,which(names(cust.clean)!="CustomerID")], upper = list(continuous = ggally_points),
        lower = list(continuous = "points"), title = "Customers After Outlier Removal")


#Scale Data
cust.scale = scale(cust.clean[-1]) 

#Number of clusters decision methods.

#Elbow method gives us an optimal group of clusters.
fviz_nbclust(cust.scale, kmeans, method = "wss") +
  geom_vline(xintercept = 6, linetype = 2)+
  labs(subtitle = "Elbow method")

#Gap statistic method gives us the count of clusters where the overlap is minimal.
fviz_nbclust(cust.scale, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

set.seed(5580)
pkm_experiment = kmeans(cust.scale, 6, 150)
fviz_cluster(pkm_experiment, data = cust.scale)

#K-Means

#Test one K-Means

set.seed(5580)
pkm = kmeans(cust.scale, 6, 150)
cust.realCenters = unscale(pkm$centers, cust.scale) 

clusteredCust = cbind(cust.clean, pkm$cluster)
#View(clusteredCust)
plot(clusteredCust[,2:6], col=pkm$cluster)
write.csv(clusteredCust, file ='customercluster1.csv',col.names = FALSE)








