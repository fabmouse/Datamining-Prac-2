#######################################
#     MODEL COMPARISON FOR AIM ONE    #
#######################################

source("Scripts/Functions.R")

data.vote <- read.csv("data/Clean Vote Data.csv", header=T)
head(data.vote)


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


# NEURAL NET --------------------------------------------------------------
#Running 3 bootstraps for the neural net classifier with 1 hidden layer
#Time Taken: ±6 seconds
#Acccuracy: 0.9044133
#               Conservative    Labour      Other       Scottish National Party
#Sensitivity    0.9763518     0.9438172     0.2781279               0.9722222
#Specificity    0.9231289     0.9820316     0.9885594               0.9667827

nn_results <- myboot(seed = 123, B = 3, model = "Neural Net")
#Stops at fourth bootstrap - not sure why

