wbcd<-read.csv("C:\\Data Science\\KNN.csv")
wbcd<-wbcd[,-1]
str(wbcd)
# table of diagnosis
prop.table(table(wbcd$diagnosis))*100
# recode diagnosis as a factor
wbcd$diagnosis <- factor(wbcd$diagnosis)

# create training and test data
wbcd_train <- wbcd[1:469, ]
wbcd_test <- wbcd[470:569, ]

# create labels for training and test data

wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

#---- Training a model on the data ----

# load the "class" library
install.packages("class")
library(class)
str(wbcd_train)
wbcd_test_pred <- knn(train = wbcd_train[,-1], 
                      test = wbcd_test[,-1],
                      cl = wbcd_train_labels, k=25)

##--------Evaluating model performance ----
# Create the cross tabulation of predicted vs. actual
table(wbcd_test_labels,wbcd_test_pred)
##ACcuracy = 93%
#######################################################

# load the library
install.packages("e1071")
library(e1071)
library(caret)

# load the dataset
data(iris)
ctrl <- trainControl(method="cv") 
my_knn_model <- train(Species ~ .,
                      method = "knn",
                      data = iris,
                      trControl=ctrl,
                      tuneGrid = expand.grid(k = c(1:100)))
my_knn_model