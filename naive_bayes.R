#### naive bayes ####

library(e1071)

# Naturally the following needs modification for your case

data.vote <- read.csv("data/Clean Vote Data.csv", header=T)
data.vote <- data.vote[,-1]

head(data.vote)
table(data.vote$Party)

fitmodel <- naiveBayes(factor(Party) ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + 
                         Vote.5 + Vote.6 + Vote.7 + Vote.8, data = data.vote)

# get class predictions
predclass <- predict(fitmodel, newdata=data.vote)

table(predclass)

# create a confusion matrix
table(predclass, data.vote$Party) #too good, maybe overfitted (?)

## bootstrap
#B: # of boostrap
B <- 1000
myboot <- function(X, Y, Model, B){
  set.seed(123)
  MSE_vector <- vector()
  for(j in 1:B){
    bs <- sample(1:length(X), length(X), replace = T)  ## bootstrap
    party_rand <- vector()
    pred <- vector()
    sqresids <- vector()
    for(i in 1:length(X)){
      party_rand[i] <- Y[bs[i]]                   ## randomised Ys
      pred[i] <- Model(X[bs[i]])            ## randomised predictions
      sqresids[i] <- (party_rand[i] - pred[i])^2   ## squared residuals   
    }
    MSE_vector[j] <- mean(sqresids)              ## MSE for this j
  }
  return(mean(MSE_vector))                       ## average of all MSE
}








