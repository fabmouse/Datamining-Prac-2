<<<<<<< HEAD
#### naive bayes ####

library(e1071)

# Naturally the following needs modification for your case

data.vote <- read.csv("data/Clean Vote Data.csv", header=T)
data.vote <- data.vote[,-1]

head(data.vote)
table(data.vote$Party)

fitmodel <- naiveBayes(factor(Party) ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + 
                         Vote.5 + Vote.6 + Vote.7 + Vote.8, data = data.vote)
datatry <- data.vote

fitmodel <- naiveBayes(factor(data.vote[,1]) ~ ., data = data.vote)


# get class predictions
predclass <- predict(fitmodel, newdata=data.vote)

table(predclass)

# create a confusion matrix
table(predclass, data.vote$Party) #too good, maybe overfitted (?)

tg## bootstrap
#B: # of boostrap
#Vote.1 + Vote.2 + Vote.3 + Vote.4 + Vote.5 + Vote.6 + Vote.7 + Vote.8

###############???

#data Bootstrap in RR
# load the library
library(caret)
# load the iris dataset
data(iris)
# define training control
train_control <- trainControl(method="boot", number=100)
# train the model
model <- train(Species~., data=iris, trControl=train_control, method="nb")
# summarize results
print(model)

#############################

# load the library
library(caret)

# define training control
train_control <- trainControl(method="boot", number=100)

# train the model
model <- train(factor(Party) ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + 
                 Vote.5 + Vote.6 + Vote.7 + Vote.8, data = data.vote,
                  trControl=train_control, method="nb")

# summarize results
print(model)




#####################

# myboot <- function(B){
#   set.seed(123)
#   
#   accuracy <- vector() #vector to store the accuracy
#   
#   for(j in 1:B){
#     bs <- sample(1:nrow(data.vote), nrow(data.vote), replace = T) ## bootstrap
#     #x_train <- data.vote[bs, 4:11] #random x - training dataset
#     #y_train <- data.vote[bs, 2] #random y - training dataset
#     vote.train <- data.vote[bs, -c(1,3)] # training dataset
#    
#     #x_test <- data.vote[-bs, 4:11] #random x - test dataset
#     #y_test <- data.vote[-bs, 2] #random y - test dataset  
#     vote.test <- data.vote[-bs, -c(1,3)] # test dataset
#     
#     predclass <- vector() #vector to store predictions
#     #TPR <- vector() #vector to store sensitivity
#     #FPR <- vector() #vector to store 1-specificity
#     
#     #for(i in 1:length(X)){
#       
#       fitmodel <- naiveBayes(factor(Party) ~ ., data = vote.train)
#       #calculate predictions
#       predclass <- predict(fitmodel, newdata=vote.test[,-1]) ## randomised predictions
#       
#       conf_mat <- table(predclass, vote.test$Party)
#       accuracy[j] <- sum(diag(conf_mat))/(sum(conf_mat)-sum(diag(conf_mat)))
#       
#    # }
#     bootstrap_acc <- mean(accuracy)              ## boostrap accuracy
#   }
#   return(bootstrap_acc)                       
# }
# 
# myboot(B =1)







=======
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








>>>>>>> 0ee3e309320e630f44745ebe2ee988410324c538
