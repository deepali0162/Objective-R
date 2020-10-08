#PCA 
wine <- read.csv("C:\\Data Science\\Assignments\\PCA\\wine.csv") #Three grape brewed wine quality analysis data set
wine <- scale(wine[,2:14])
wine.pca<-prcomp(wine,scale.=TRUE)
summary(wine.pca)  # SVDValue and relative variance
wine.pca$rotation #loading
wine.pca$x #score
screeplot(wine.pca)  # variance distribution map
biplot(wine.pca,scale=F)  # Draw x and rotation directly without standardization

library(devtools)
install_github("vqv/ggbiplot")

install.packages("ggbiplot")
library(ggbiplot)
ggbiplot(wine.pca, obs.scale = 1, var.scale = 1,
         groups = NULL, ellipse = TRUE, circle = TRUE) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')
##As per the bbgiplot, we are getting 3 clusters for WINE Dataset.

install.packages('factoextra')
library("factoextra")
##HIERARCHICAL CLUSTERING
data_hier <- dist(wine, method = "euclidean") #Computing the distance natrix

fit <- hclust(data_hier, method="average") # Building the algorithm 
plot(fit) #display dendogram

rect.hclust(fit, k=3, border="red")
#OR
fviz_dend(fit, cex = 0.5) # cex: label size
# Cut in 5 groups and color by groups
fviz_dend(fit, k = 3, # Cut in three groups
          cex = 0.5, # label size
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE) # Add rectangle around groups
#Attach the cluster numbers to WineData
clusters<-cutree(fit,k=3)
Hier_output=data.frame('WineData'=wine[,1],'Cluster' =clusters)
View(Hier_output)

##K MEANS CLUSTERING
fviz_nbclust(wine[,-1], kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
km <- kmeans(wine[,-1],3) 
km$centers
km$cluster
KMean_Ouput<-data.frame("WineData"=wine[,1],"cluster"=km$cluster)
View(KMean_Ouput)

##Considering all the three methods, I conclude the number of clusters should be three.



