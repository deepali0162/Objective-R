# Libraries
library(naivebayes)
library(ggplot2)
library(caret)
library(psych)
library(e1071)

# Data(Train)
train_sal <- read.csv(file.choose())
str(train_sal)
View(train_sal)
train_sal$Salary <- as.factor(train_sal$Salary)
class(train_sal)

# Data(Test)
test_sal <- read.csv(file.choose())
str(test_sal)
View(test_sal)
test_sal$Salary <- as.factor(test_sal$Salary)
class(test_sal)

train_sal$Salary <- factor(train_sal$Salary)
#Visualization 
# Plot and ggplot 
ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$age, fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

#Density Plot 
ggplot(data=train_sal,aes(x = train_sal$age, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Age - Density Plot")

# Naive Bayes Model 
Model <- naiveBayes(train_sal$Salary ~ ., data = train_sal)
Model

Model_pred <- predict(Model,test_sal)

table(Model_pred,test_sal$Salary)
# Model_pred  <=50K  >50K
# <=50K  10550  1911
# >50K     810  1789
# ~82%

##  model performance ----
Model2 <- naiveBayes(train_sal, train_sal$Salary, laplace = 1)
Model_pred2 <- predict(Model2, test_sal)
table(Model_pred2, test_sal$Salary)
# Model_pred2  <=50K  >50K
# <=50K  11185    97
# >50K     175  3603
# ~98%