library(readr)
library(e1071)
letterdata <- read_csv("C:/Data Science/letterdata.csv")
letterdata$letter<-as.factor(letterdata$letter)
# divide into training and test data
letters_train <- letterdata[1:16000, ]
letters_test  <- letterdata[16001:20000, ]
##Training a model on the data ----
# begin by training a simple linear SVM
install.packages("kernlab")
library(kernlab)
##Linear method
letter_classifier <- ksvm(letter ~ ., data = letters_train,
                          kernel = "vanilladot")
View(letter_classifier)
## Evaluating model performance ----
# predictions on testing dataset
letter_predictions <- predict(letter_classifier, letters_test)
head(letter_predictions)
#table(letter_predictions, letters_test$letter)
agreement <- letter_predictions == letters_test$letter
prop.table(table(agreement))
## Improving model performance ----
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)
head(letter_predictions_rbf)
agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))