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

#Set up the cross validation to select the optimal parameters
set.seed(123)
fitControl <- trainControl(method = "cv", number = 5)

# CLASSIFICATION TREE -----------------------------------------------------


# DECISION TREE -----------------------------------------------------------


# RANDOM FOREST -----------------------------------------------------------
#Lei found that a random forest with mtry = 4 and ntree = 470 works best.

#Running 1000 bootstraps for the random forest classifier 
#Time Taken: ± 246
#Acccuracy:  0.9625732
#               Conservative    Labour      Other       Scottish National Party
#Sensitivity    0.9757830       0.9659925   0.8327807   0.9998667
#Specificity    0.9827818       0.9826410   0.9843351   0.9964565

rf_results <- myboot(seed = 123, B = 1000, rf_mtry = 4, rf_ntree = 470,
                     model = "Random Forest")

# NAIVE BAYES -------------------------------------------------------------
#Running 1000 bootstraps for the naive bayes classifier 
#Time Taken: ±80 seconds
#Acccuracy: 0.7513753
#               Conservative    Labour      Other       Scottish National Party
#Sensitivity    0.9145480     0.6604991     0.03145755               1.0000000
#Specificity    0.9572252     0.9848712     0.95232648               0.8169477

nbfit <- train(factor(Party) ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 + 
                 Voting.5 + Voting.6 + Voting.7 + Voting.8, data = data.vote, 
               method = "nb",
               trControl = fitControl)

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

svm_cost <- train(factor(Party) ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 + 
                    Voting.5 + Voting.6 + Voting.7 + Voting.8, data = data.train, 
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