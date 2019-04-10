#### support vector machines for multiple classes 

library(e1071)
data.vote <- read.csv("data/Clean Vote Data.csv", header=T)
ind <- sample(1:nrow(data.vote), 0.6 * nrow(data.vote), replace = FALSE) ## bootstrap
data.train <- data.vote[ind, ] # training dataset
data.test <- data.vote[-ind, ] # test dataset

set.seed(123)

#Set up the support vector machine
#Kernel = 
svmfit <- svm(factor(Party) ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + 
                Vote.5 + Vote.6 + Vote.7 + Vote.8, data = data.train,
              kernel = "linear", cost = 10, gamma = 1)

#Try a range of cost parameters
set.seed(123)
tunePars = tune(svm, factor(Party) ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + 
                  Vote.5 + Vote.6 + Vote.7 + Vote.8, data = data.train, 
                kernel ="linear",
                ranges = list(cost = c(0.001, 0.01, 0.1, 1,5, 10, 100)))

summary(tunePars)

bestModel<- tunePars$best.model
summary (  bestModel)

#Obtain predictions and create the confusion matrix
predclass <- predict(bestModel, data.test[, 4:11])
conf_mat <- table(observed = data.test$Party, predicted = predclass)

fit_metrics <- vector("list", length(levels(data.test$Party)))
for (i in seq_along(fit_metrics)) {
  positive.class <- levels(data.test$Party)[i]
  # in the i-th iteration, use the i-th class as the positive class
  fit_metrics[[i]] <- confusionMatrix(predclass, data.test$Party, 
                                      positive = positive.class)
}

#Accuracy
sum(diag(conf_mat))/(sum(conf_mat))

#Sensitivity and Specificity
fit_metrics[[1]]$byClass[, "Sensitivity"]
fit_metrics[[1]]$byClass[, "Specificity"]
