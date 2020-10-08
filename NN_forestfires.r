#Load Data Set
library(readr)
forestfires_1_ <- read_csv("Desktop/Data Science/Assignments/Neural NEtworks/forestfires (1).csv")
View(forestfires_1_)

#custom Normalize Function
normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

#apply normalisation to entire dataset
forest_norm <- as.data.frame(lapply(forestfires_1_[3:11], normalise))

#creating train and test dataset
forest_tr <- forest_norm[1:400,]
forest_ts <- forest_norm[401:517,]

#training a model on the data
library(neuralnet)
# simple ANN with only a single hidden neuron
model1 <- neuralnet(area ~ . , data = forest_tr, act.fct = "logistic")
plot(model1)

#evaluating model performance
model1_results <- compute(model1,forest_ts[1:8])
predicted_strength1 <- model1_results$net.result
cor(predicted_strength1,forest_ts$area)

# simple ANN with only a single hidden neuron
model <- neuralnet(area ~ .,data=forest_tr,act.fct = "logistic")
plot(model) ##Steps = 127 and Error = 0.595518
model_results <- compute(model,forest_ts[1:8])
predicted_strength <- model_results$net.result
cor(predicted_strength,forest_ts$area) #0.01591237

# simple ANN with 10 hidden neuron
model1 <- neuralnet(area ~FFMC+DMC+DC+ISI+temp+RH+wind+rain  , 
                    data = forest_tr, act.fct = "logistic",hidden=c(10,2))
plot(model1) ##Steps = 115 and Error = 0.586372
#evaluating model performance
model1_results <- compute(model1,forest_ts[1:8])
predicted_strength1 <- model1_results$net.result
cor(predicted_strength1,forest_ts$area) #0.06134951

# simple ANN with 5 hidden neuron
model2 <- neuralnet(area ~FFMC+DMC+DC+ISI+temp+RH+wind+rain  , 
                    data = forest_tr, act.fct = "logistic",hidden=c(5,3))
plot(model2) ##Steps = 68 and Error = 0.58917
#evaluating model performance
model2_results <- compute(model2,forest_ts[1:8])
predicted_strength2 <- model2_results$net.result
cor(predicted_strength2,forest_ts$area) #0.09565786
