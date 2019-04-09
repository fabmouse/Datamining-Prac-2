#### naive bayes ####

library(e1071)

# Naturally the following needs modification for your case
#Import the data
data.vote <- read.csv("data/Clean Vote Data.csv", header=T)
head(data.vote)
table(data.vote$Party)


# SENSITIVITY -------------------------------------------------------------
fitmodel <- naiveBayes(factor(Party) ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + 
                         Vote.5 + Vote.6 + Vote.7 + Vote.8, data = data.vote)
predclass <- predict(fitmodel, newdata=data.vote)

# create a confusion matrix

conf_matrix <- table(data.vote$Party, predclass)

values <- function(inputmatrix){
  sens <- c()
  spec <- c()
  
  for(i in 1:nrow(inputmatrix)){
    TP <- inputmatrix[i,i]
    TN <- sum(diag(inputmatrix[i,i])) - TP
    FP <- sum(inputmatrix[,i]) - TP
    FN <- sum(inputmatrix[i,]) - TP
    
    sens[i] <- TP / (TP + FN)
    spec[i] <- TN / (TN + FP)
    }
  
  return(list(Sensitivity = sens, Specificity = spec))
}

library(caret) # for confusionMatrix function
cm <- vector("list", length(levels(data.vote$Party)))
for (i in seq_along(cm)) {
  positive.class <- levels(data.vote$Party)[i]
  # in the i-th iteration, use the i-th class as the positive class
  cm[[i]] <- confusionMatrix(predclass, data.vote$Party, 
                             positive = positive.class)
}

metrics <- c("Precision", "Recall")
print(cm[[1]]$byClass[, metrics])



# Trying without validation ----------------------------------------------------
#ON ALL DATA
#Fit the Naive Bayes
fitmodel <- naiveBayes(factor(Party) ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + 
                         Vote.5 + Vote.6 + Vote.7 + Vote.8, data = data.vote)

# get class predictions
predclass <- predict(fitmodel, newdata=data.vote)

table(predclass)

# create a confusion matrix
table(predclass, data.vote$Party) #too good, maybe overfitted (?)

#ON TEST AND TRAIN DATA

sample_indices <- sample(1:nrow(data.vote), nrow(data.vote), replace = FALSE)
vote.train <- data.vote[sample_indices, ]
vote.test <- data.vote[-sample_indices, ]

#Fit the Naive Bayes
fitmodel <- naiveBayes(factor(Party) ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + 
                         Vote.5 + Vote.6 + Vote.7 + Vote.8, data = vote.train)

# get class predictions
predclass <- predict(fitmodel, newdata = vote.train[, 4:11])

table(predclass)

# create a confusion matrix
table(predclass, vote.train$Party) #too good, maybe overfitted (?)

#GLM

mod.glm <- glm(Party ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + 
                 Vote.5 + Vote.6 + Vote.7 + Vote.8, data = data.factor)


## bootstrap
#B: # of boostrap
#Vote.1 + Vote.2 + Vote.3 + Vote.4 + Vote.5 + Vote.6 + Vote.7 + Vote.8

# With Caret Package -----------------------------------------------------------

# load the library
library(caret)

# define training control
train_control <- trainControl(method = "boot", number = 1)

# train the model
model <- train(Party ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 +
                 Vote.5 + Vote.6 + Vote.7 + Vote.8, data = data.vote,
                  trControl = train_control, method = "nb")

# summarize results
print(model)

# With Own Function ------------------------------------------------------------

myboot <- function(B){
  set.seed(123)

  accuracy <- vector() #vector to store the accuracy

  for(j in 1:B){
    bs <- sample(1:nrow(data.vote), nrow(data.vote), replace = T) ## bootstrap
    #x_train <- data.vote[bs, 4:11] #random x - training dataset
    #y_train <- data.vote[bs, 2] #random y - training dataset
    vote.train <- data.vote[bs, -c(1,3)] # training dataset

    #x_test <- data.vote[-bs, 4:11] #random x - test dataset
    #y_test <- data.vote[-bs, 2] #random y - test dataset
    vote.test <- data.vote[-bs, -c(1,3)] # test dataset

    predclass <- vector() #vector to store predictions
    #TPR <- vector() #vector to store sensitivity
    #FPR <- vector() #vector to store 1-specificity

    #for(i in 1:length(X)){

    fitmodel <- naiveBayes(factor(Party) ~ ., data = vote.train)
    #calculate predictions
    predclass <- predict(fitmodel, newdata=vote.test[,-1]) ## randomised predictions

    conf_mat <- table(predclass, vote.test$Party)
    accuracy[j] <- sum(diag(conf_mat))/(sum(conf_mat))

   # }

  }
  
  bootstrap_acc <- mean(accuracy)              ## boostrap accuracy
  return(bootstrap_acc)
}

myboot(B = 1)

