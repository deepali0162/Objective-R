install.packages('factoextra')
library("factoextra")

#Data load
data1 <- read.csv("C:\\Data Science\\Assignments\\Clustering\\crime_data.csv")
View(data1)
#data standarization
crimedata <- scale(data1[,2:5])

##HIERARCHICAL CLUSTERING
d <- dist(crimedata, method = "euclidean") #Computing the distance natrix

fit <- hclust(d, method="average") # Building the algorithm 
plot(fit) #display dendogram

rect.hclust(fit, k=5, border="red")
#OR
fviz_dend(fit, cex = 0.5) # cex: label size
# Cut in 5 groups and color by groups
fviz_dend(fit, k = 5, # Cut in five groups
          cex = 0.5, # label size
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE) # Add rectangle around groups
#Attach the cluster numbers to CrimeData
clusters<-cutree(fit,k=5)
Final_output=data.frame('CrimeData'=data1[,1],'Cluster' =clusters)
View(Final_output)

##K MEANS CLUSTERING
fviz_nbclust(data1[,-1], kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
km <- kmeans(data1[,-1],5) 
km$centers
km$cluster
clust<-data.frame("Airlines"=data1[,1],"cluster"=km$cluster)
View(clust)
