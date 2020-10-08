install.packages('factoextra')
library("factoextra")

#Data load
mydata1 <- read.csv("C:\\Data Science\\Assignments\\Clustering\\EastWestAirlines.csv")

#data standarization
airdata <- scale(mydata1[,2:12])

##HIERARCHICAL CLUSTERING
d <- dist(airdata, method = "euclidean") #Computing the distance natrix

fit <- hclust(d, method="average") # Building the algorithm 
plot(fit) #display dendogram

fit1 <- hclust(d, method="complete") # Building the algorithm 
plot(fit1) #display dendogram

#I am considering method=complete because I'm getting more clear with this method
rect.hclust(fit1, k=5, border="red")

# #fviz_dend(fit, cex = 0.5) # cex: label size
# # Cut in 4 groups and color by groups
# fviz_dend(fit1, k = 5, # Cut in four groups
#           cex = 0.5, # label size
#           color_labels_by_k = TRUE, # color labels by groups
#           rect = TRUE # Add rectangle around groups
# )

#Attach the cluster numbers to Airlines
clusters<-cutree(fit1,k=5)
Final_output=data.frame('AirLines'=mydata1[,1],'Cluster' =clusters)
View(Final_output)

##K MEANS CLUSTERING
fviz_nbclust(mydata1[,-1], kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
km <- kmeans(mydata1[,-1],5) 
km$centers
km$cluster
clust<-data.frame("Airlines"=mydata1[,1],"cluster"=km$cluster)
View(clust)
