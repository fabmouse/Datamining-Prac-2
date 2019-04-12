###################################################
#               FINAL AIM ONE SCRIPT              #
###################################################

source("Scripts/Functions.R")

data.vote <- read.csv("data/remainORleave.csv", header = T)
head(data.vote)


# Set up the datasets -----------------------------------------------------
#Split dataset into trainging and testing
set.seed(123)
ind <- sample(1:nrow(data.vote), 0.75 * nrow(data.vote), replace = FALSE) 
data.train <- data.vote[ind, -c(2:5)] # training dataset to choose best model
data.validation <- data.vote[-ind, -c(2:5)] # validation dataset to evaluate best model

#Set up the cross validation to select the optimal parameters
set.seed(123)
fitControl <- trainControl(method = "cv", number = 5)

# CLASSIFICATION TREE -----------------------------------------------------
#Divide training dataset into two parts
set.seed(123)
tuneind <- sample(1:nrow(data.train), 0.75 * nrow(data.train), replace = FALSE) 
tune.train <- data.train[tuneind, ]
tune.test <- data.train[-tuneind, ]

# Create initial logistic regression
## X = Voting data Y = Party, dataset = train
original.lr <- multinom(X...Party ~ ., data = tune.train)
summary(original.lr)
## when the X only include the voting data, the residual deviance = 153.3819 , 
#AIC = 207.3819 

#Calculate accuracy for the original logistic regression 
## Build the confusion matrix for original.lr by test dataset
prediction <- predict(original.lr, test)
actuals <- test$Party
CMtable <- table(actuals, prediction)
print(CMtable)
# Accuracy and Misclass rate
accuracy.lr <- sum(diag(CMtable))/sum(CMtable)
misclass.lr <- 1-sum(diag(CMtable))/sum(CMtable)
print(accuracy.lr) # Accuracy = 94.48%
print(misclass.lr) # Misclass rate = 5.52%

# RANDOM FOREST -----------------------------------------------------------
#Lei found that a random forest with mtry = 4 and ntree = 470 works best.
library(randomForest)
source("Scripts/RFFunction.R")

# Initial Look: Fit a RF on the training set
fit.rf <- randomForest(factor(X...Party) ~ ., data = data.train)
print(fit.rf)

##Now improve parameters
## Step 1: Try to find a good mtry number using findgoodmtry function
n <- length(names(data.train[, -1]))
findgoodmtry(data.train[, -1], data.train[, 1], n , 123,  data.train)
## Error calculated as Out Of Bag error rate
## Both output and plot suggest mtry = 4, the error is minimum

## Step 2: try to find a good tree number
set.seed(123)
fit.rf <- randomForest(factor(X...Party)~ ., data = data.train, mtry = 4, 
                       ntree = 1000)
plot(fit.rf)
## when number of trees is around 300 to 400, the model is stable

## Step 3: set ntree = 350  mtry = 4, to get the basic information about these 
# paramters. 
set.seed(123)
select.rf <- randomForest(factor(X...Party) ~ ., data = data.train,
                          mtry = 4, ntree = 350)
print(select.rf)
## when ntree = 350, the OOB err is 3.77%

# Step 4: Try to find the good number of trees, when mtry = 4
findgoodntree(data.train[, -1],  data.train[, 1], start = 300, end = 550, by = 20,
              mtrynumber = 4, data.train)
findgoodntree(data.train[, -1],  data.train[, 1], start = 400, end = 550, by = 10,
              mtrynumber = 4, data.train)
# when mtry = 4, ntrr = 460, the accuracy = 0.9800443, OOBError = 0.04806494
# the misclass.rate = 0.01995565

#Best model 
rf_model <-  randomForest(factor(X...Party) ~ ., data = data.train, 
                          mtry = 4, ntree = 460)

importance(rf_model) #Shows the importance of each vote
varImpPlot(rf_model) #Shows the importance of each vote in a plot
plot(rf_model) #Shows model performance?

#Now perform validation
predclass <- predict(rf_model, newdata = data.validation[, -1]) 

rf_fit_metrics <- vector("list", length(levels(data.validation$X...Party)))
for (i in seq_along(rf_fit_metrics)) {
  positive.class <- levels(data.validation$X...Party)[i]
  # in the i-th iteration, use the i-th class as the positive class
  rf_fit_metrics[[i]] <- confusionMatrix(predclass, data.validation$X...Party,
                                      positive = positive.class)
}

# LOGISTIC REGRESSION -----------------------------------------------------



# NAIVE BAYES -------------------------------------------------------------
nb_params <- train(factor(Party) ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 + 
                     Voting.5 + Voting.6 + Voting.7 + Voting.8, data = data.train, 
                   method = "nb",
                   trControl = fitControl)

# nb_results <- myboot(seed = 123, B = 10, model = "Naive Bayes")
nb_model <- naiveBayes(factor(Party) ~ ., data = data.train)

predclass <- predict(nb_model, newdata = data.validation[, -1]) 

nb_fit_metrics <- vector("list", length(levels(data.validation$Party)))
for (i in seq_along(nb_fit_metrics)) {
  positive.class <- levels(data.validation$Party)[i]
  nb_fit_metrics[[i]] <- confusionMatrix(predclass, data.validation$Party,
                                      positive = positive.class)
}

# SUPPORT VECTOR MACHINE --------------------------------------------------
svm_params <- train(factor(Party) ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 + 
                      Voting.5 + Voting.6 + Voting.7 + Voting.8, data = data.train, 
                    method = "svmRadialCost",
                    trControl = fitControl)
svm_cost <- svm_params$bestTune$C
svm_model <- svm(factor(Party) ~ ., data = data.train,
                type = "C", kernel = "radial", cost = svm_cost, gamma = 1)

predclass <- predict(svm_model, newdata = data.validation[, -1]) 

svm_fit_metrics <- vector("list", length(levels(data.validation$Party)))
for (i in seq_along(svm_fit_metrics)) {
  positive.class <- levels(data.validation$Party)[i]
  svm_fit_metrics[[i]] <- confusionMatrix(predclass, data.validation$Party,
                                      positive = positive.class)
}

# NEURAL NET --------------------------------------------------------------
nn_params <- train(factor(Party) ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 + 
                     Voting.5 + Voting.6 + Voting.7 + Voting.8, data = data.train, 
                   method = "nnet",
                   trControl = fitControl)
nn_hidden <- nn_params$bestTune$size

nn_model <- neuralnet(as.factor(Party) ~ ., data = data.train,
                      linear.output = FALSE, hidden = nn_hidden, lifesign = "full")

predclass <- predict(nn_model, newdata = data.validation[, -1]) 

nn_fit_metrics <- vector("list", length(levels(data.validation$Party)))
for (i in seq_along(nn_fit_metrics)) {
  positive.class <- levels(data.validation$Party)[i]
  nn_fit_metrics[[i]] <- confusionMatrix(predclass, data.validation$Party,
                                      positive = positive.class)
}

# Plot of Kappa Comparisons -----------------------------------------------
resamps <- resamples(list(RF = rf_params, 
                          NB = nb_params, 
                          SVM = svm_params, 
                          NN = nn_params))
summary(resamps)
# Look for plots
trellis.par.set(caretTheme())
dotplot(resamps, metric = "Accuracy")
dotplot(resamps, metric = "Kappa")

# PLOT COMPARING PARTY SENSITIVITY ----------------------------------------
rf_Sens <- rf_fit_metrics[[1]]$byClass[, "Sensitivity"]
nb_Sens <- nb_fit_metrics[[1]]$byClass[, "Sensitivity"]
svm_Sens <- svm_fit_metrics[[1]]$byClass[, "Sensitivity"]
nn_Sens <- nn_fit_metrics[[1]]$byClass[, "Sensitivity"]

plot(x = 1:4, y = svm_Sens, ylim = c(0, 1), xlab = "Party", ylab = "Sensitivity",
     main = "Sensitivity Comparison Based on Validation Set",
     type = "b", col = "red", xaxt = "n")
axis(1, at = seq(1, 4, by = 1), labels = c("CON", "LAB", "OTH", "SNP"))
points(x = 1:4, y = nb_Sens, type = "b", col = "blue")
points(x = 1:4, y = rf_Sens, type = "b", col = "green")
points(x = 1:4, y = nn_Sens, type = "b", col = "orange")
legend("bottomleft", legend = c("SVM", "NB", "RF", "NN"), 
       col = c("red", "blue", "green", "orange"), lwd = 1, cex = 0.75)


