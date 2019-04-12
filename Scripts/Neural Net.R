###################
#NEURAL NET SCRIPT#
###################

library(neuralnet)
library(ggplot2)
source("Scripts/Functions.R")

data.vote <- read.csv("data/Clean Vote Data.csv", header = TRUE)
head(data.vote)

nn_boot <- myboot(B = 3, model = "Neural Net")

plot <- ggplot(nn_boot$Acc_All, aes())


nn.pred <- prediction(party_predictions, data.test$Party)
pref <- performance(nn.pred, "tpr", "fpr")
plot(pref)

#This fits a multinomial log loinear model via neural nets
mn.net <- nnet::multinom(as.factor(Party) ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + 
                           Vote.5 + Vote.6 + Vote.7 + Vote.8, data = data.train, 
                         model = TRUE)
party.predictions <- predict(mn.net, newdata=data.test, type="prob")
multiclass.roc(data.test$Party, party.predictions)

# WORKING ----------------------------------------------------------------------
set.seed(123)
ind <- sample(1:nrow(data.vote), 0.6*nrow(data.vote), replace = FALSE)
data.train <- data.vote[ind, ]
data.test <- data.vote[-ind, ]

# estimate a neural network with one hidden layer of 8 nodes
nn <- neuralnet(as.factor(Party) ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + 
                  Vote.5 + Vote.6 + Vote.7 + Vote.8, data = data.train,
                linear.output = FALSE, hidden = 1, lifesign="full")
plot(nn)

predict_testNN <- compute(nn, data.test[, c(4:11)])

#Seperate into 4 parties
party_predictions <- vector()

for(i in 1:nrow(predict_testNN$net.result)){
  result_row <- predict_testNN$net.result[i, ]
  max_ind <- which.max(result_row)
  if(max_ind == 1) party_predictions[i] = "Conservative"
  if(max_ind == 2) party_predictions[i] = "Labour"
  if(max_ind == 3) party_predictions[i] = "Other"
  if(max_ind == 4) party_predictions[i] = "Scottish National Party"
}

conf_mat <- table(data.test$Party, party_predictions)
accuracy <- sum(diag(conf_mat))/(sum(conf_mat))



