library(ggplot2)
library(GGally)
library(DMwR) 
library(factoextra) # clustering algorithms & visualization
library(NbClust)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms

set.seed(5580)

#Original Data

prod = read.csv('productcluster.csv')
View(prod)

ggpairs(prod[,which(names(prod)!="StockCode")], upper = list(continuous = ggally_points),
        lower = list(continuous = "points"), title = "Products Before Outlier Removal")

#Clean Data - Remove Outliers

prod.clean <- prod[prod$StockCode != "47556B", ]
prod.clean <- prod.clean[prod.clean$StockCode != "23005", ]
prod.clean <- prod.clean[prod.clean$StockCode != "84568", ]
prod.clean <- prod.clean[prod.clean$StockCode != "DOT", ]

View(prod.clean)

ggpairs(prod.clean[,which(names(prod.clean)!="StockCode")], upper = list(continuous = ggally_points),
        lower = list(continuous = "points"), title = "Products After Outlier Removal")

#Scale Data

prod.scale = scale(prod.clean[-1]) 

#Plot Clusters

fviz_nbclust(prod.scale, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)+
  labs(subtitle = "Elbow method")

fviz_nbclust(prod.scale, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method") #Checks for the lowerst value which indicates low overlaps

set.seed(5580)
pkm_experiment = kmeans(prod.scale, 5, 150)
fviz_cluster(pkm_experiment, data = prod.scale)

#K-Means

#Test one K-Means

set.seed(5580)
pkm = kmeans(prod.scale, 5, 150)
prod.realCenters = unscale(pkm$centers, prod.scale) 

clusteredProd = cbind(prod.clean, pkm$cluster)
#View(clusteredProd)
plot(clusteredProd[,2:5], col=pkm$cluster)
write.csv(clusteredProd, file ='productcluster1.csv',col.names = FALSE)
