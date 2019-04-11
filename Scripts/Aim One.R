#######################################
#     MODEL COMPARISON FOR AIM ONE    #
#######################################

source("Scripts/Functions.R")

data.vote <- read.csv("data/Clean Vote Data.csv", header=T)
head(data.vote)

#Split dataset into trainging and testing
set.seed(123)
ind <- sample(1:nrow(data.vote), 0.6 * nrow(data.vote), replace = FALSE) ## bootstrap
data.train <- data.vote[ind, ] # training dataset
data.test <- data.vote[-ind, ] # test dataset

# CLASSIFICATION TREE -----------------------------------------------------


# DECISION TREE -----------------------------------------------------------


# RANDOM FOREST -----------------------------------------------------------


# NAIVE BAYES -------------------------------------------------------------
#Running 1000 bootstraps for the naive bayes classifier 
#Time Taken: ±80 seconds
#Acccuracy: 0.7513753
#               Conservative    Labour      Other       Scottish National Party
#Sensitivity    0.9145480     0.6604991     0.03145755               1.0000000
#Specificity    0.9572252     0.9848712     0.95232648               0.8169477

nb_results <- myboot(seed = 123, B = 1000, model = "Naive Bayes")

# SUPPORT VECTOR MACHINE --------------------------------------------------
#Running 1000 bootstraps for the naive bayes classifier 
#Time Taken: ±50 seconds
#Acccuracy: 0.9452647
#               Conservative    Labour      Other       Scottish National Party
#Sensitivity    0.9868715     0.9315431   0.7865079               0.9465812
#Specificity    0.9280813     0.9871589   0.9918741               1.0000000

#Select the best cost parameter using cross validation 
#Using the caret package for carrying out 5 fold CV and Cost selection
set.seed(123)
fitControl <- trainControl(method = "cv", number = 5)
svm_cost <- train(factor(Party) ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + 
                    Vote.5 + Vote.6 + Vote.7 + Vote.8, data = data.train, 
                  method = "svmRadialCost",
                  trControl = fitControl)
best_cost <- svm_cost$bestTune$C

svm_results <- myboot(seed = 123, B = 1000, model = "SVM", svm_cost = best_cost)

# NEURAL NET --------------------------------------------------------------
#Running 3 bootstraps for the neural net classifier with 1 hidden layer
#Time Taken: ±6 seconds
#Acccuracy: 0.9044133
#               Conservative    Labour      Other       Scottish National Party
#Sensitivity    0.9763518     0.9438172     0.2781279               0.9722222
#Specificity    0.9231289     0.9820316     0.9885594               0.9667827

nn_results <- myboot(seed = 123, B = 3, nn_hidden = 1, model = "Neural Net")
#Stops at fourth bootstrap - not sure why