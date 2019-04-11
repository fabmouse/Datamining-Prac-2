#######################################
#     MODEL COMPARISON FOR AIM TWO   #
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
#Time Taken:
#Acccuracy:
#               Conservative    Labour      Other       Scottish National Party
#Sensitivity    
#Specificity    


# SUPPORT VECTOR MACHINE --------------------------------------------------
#Running 1000 bootstraps for the naive bayes classifier 
#Time Taken: 
#Acccuracy:
#               Conservative    Labour      Other       Scottish National Party
#Sensitivity    
#Specificity   


# NEURAL NET --------------------------------------------------------------
#Running 3 bootstraps for the neural net classifier with 1 hidden layer
#Time Taken: 
#Acccuracy: 
#               Conservative    Labour      Other       Scottish National Party
#Sensitivity    
#Specificity    
