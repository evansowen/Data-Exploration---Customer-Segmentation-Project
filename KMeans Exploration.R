# Load libraries and data
library(data.table)
library(lubridate)
library(bit64)
library(dplyr)
library(stringr)
library(anytime)
library(tidyverse)
library(ggplot2)
library(broom)
library(janitor)
library(dlookr)
library(purrr)
library(psych)
library(openxlsx)
library(expss)
library(cluster)    # clustering algorithms
library(factoextra)

setwd("~/Desktop/Segmentation Project/Working Folder")
df <- read.xlsx(file.choose(), 1)

# First Trial - Good Clusters
# Feature Selection,   would like to find high value customer segements based upon ARPU or Revenue

# Choose Select Variables
# Needs to be numeric,  no outliers.  Scale numerical data.
df1 <- select(df, Age, EducationYears, TransHHIncome, EmploymentLength, Total_Revenue)

# Scale/Normalize Data
df1.scaled <- scale(df1)

# Determine best number of clusters
fviz_nbclust(df1.scaled, kmeans, method = "wss")
fviz_nbclust(df1.scaled, kmeans, method='silhouette')
nc <- NbClust(df1.scaled, min.nc=3, max.nc=5, method="kmeans") # too long
table(nc$Best.nc[1,]) # 
barplot(table(nc$Best.nc[1,]))

# Determine Optimal # Clusters then run Kmeans Algorithm
set.seed(1234)
km1 <- kmeans(df1.scaled, 3, nstart=25) # Kmeans results
kcluster=km1$cluster

kmeans.med1 <- aggregate(as.data.frame(df1.scaled), 
                         by=list(cluster=kcluster), median)  # aggregate data, median of each cluster

kmeans.med2 <- aggregate(as.data.frame(df1), 
                        by=list(cluster=kcluster), median)  # aggregate data, median of each cluster

df.w_clusters <- cbind(df1, cluster = km1$cluster)  # add clusters to original DF

fviz_cluster(km1, data = df1, geom="point",  
             main = paste ("Kmeans Cluster Plot"))  # Visualize Clusters w/PCA

km1$size  # identification of size of clusters

# parallell coordinates plot
ggparcoord(data=df1.scaled,
           columns = 1:4, groupColumn = 5, order = "anyClass",
           showPoints = TRUE, 
           title = "Parallel Coordinate Plot for the Iris Data",
           alphaLines = 0.3) + 
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme(
    plot.title = element_text(size=10)
  )


# Choose Select Variables
# Needs to be numeric,  no outliers.  Scale numerical data.
df1 <- select(df, Age, EducationYears, TransHHIncome, EmploymentLength, Total_Revenue)

# Scale/Normalize Data
df1.scaled <- scale(df1)

# Determine best number of clusters
fviz_nbclust(df1.scaled, kmeans, method = "wss")
fviz_nbclust(df1.scaled, kmeans, method='silhouette')
nc <- NbClust(df1.scaled, min.nc=3, max.nc=5, method="kmeans") # too long
table(nc$Best.nc[1,]) # 
barplot(table(nc$Best.nc[1,]))

# Determine Optimal # Clusters then run Kmeans Algorithm
set.seed(1234)
km1 <- kmeans(df1.scaled, 3, nstart=25) # Kmeans results
kcluster=km1$cluster

kmeans.med1 <- aggregate(as.data.frame(df1.scaled), 
                         by=list(cluster=kcluster), median)  # aggregate data, median of each cluster

kmeans.med2 <- aggregate(as.data.frame(df1), 
                         by=list(cluster=kcluster), median)  # aggregate data, median of each cluster

df.w_clusters <- cbind(df1, cluster = km1$cluster)  # add clusters to original DF

fviz_cluster(km1, data = df1, geom="point",  
             main = paste ("Kmeans Cluster Plot"))  # Visualize Clusters w/PCA

km1$size  # identification of size of clusters

# parallell coordinates plot
ggparcoord(data=df1.scaled,
           columns = 1:4, groupColumn = 5, order = "anyClass",
           showPoints = TRUE, 
           title = "Parallel Coordinate Plot for the Iris Data",
           alphaLines = 0.3) + 
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme(
    plot.title = element_text(size=10)
  )

