Zoo <- read.csv("C:\\Data Science\\Assignments\\KNN\\Zoo.csv")
str(Zoo)
Zoo <- Zoo[,-1]
# table of diagnosis
prop.table(table(Zoo$type))*100
# recode diagnosis as a factor
Zoo$type <- factor(Zoo$type)

#Creating training and testing dataset
ZooTrain<- Zoo[1:70, ]
ZooTest <- Zoo[71:101, ]

#Create labels for train and test data
Zoo_train_labels <- Zoo[1:70, 17]
Zoo_test_labels <- Zoo[71:101, 17]

# cl <- Zoo_train_labels
#Preparing model
library(class)
# colnames()
Zoo_test_pred <- knn(train = ZooTrain[,-1], 
                      test = ZooTest[,-1],
                      cl = Zoo_train_labels, k=21)

# load the dataset
data<- read.csv("C:\\Data Science\\Assignments\\KNN\\Zoo.csv")
ctrl <- trainControl(method="cv") 
my_knn_model <- train(Species ~ .,
                      method = "knn",
                      data = iris,
                      trControl=ctrl,
                      tuneGrid = expand.grid(k = c(1:100)))
my_knn_model
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was k = 21.
