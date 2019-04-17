#######################################
#          SCRIPT FOR AIM TWO         #
#######################################

# LIBRARIES ---------------------------------------------------------------
library(caret)        #For training with caret package
library(caret)        #For the confusion matrix
library(tree)         #For the classification tree
library(ISLR)         #For the classification tree
library(rpart)        #For the classification tree
library(rpart.plot)   #For the classification tree
library(randomForest) #For the random forest
library(nnet)         #For logistic regression
library(e1071)        #For the support vector machine
library(neuralnet)    #For the neural net
library(pROC)         #For ROC curves
library(ROSE)         #For ROC curves
library(dplyr)

# Set up the datasets -----------------------------------------------------
data.vote <- read.csv("data/Final Vote Data.csv", header = TRUE)
data.vote <- na.omit(data.vote)
data.vote$Constituency <- as.numeric(data.vote$Constituency)
head(data.vote)

#Split dataset into a training set (75%) and hold back (25%) for validation
set.seed(123)
ind <- sample(1:nrow(data.vote), 0.75 * nrow(data.vote), replace = FALSE) 
data.train <- data.vote[ind, -c(2, 4)]
data.validation <- data.vote[-ind,  -c(2, 4)] 

#Split training dataset into a training set (75%) and testing set (25%)
set.seed(123)
tuneind <- sample(1:nrow(data.train), 0.75 * nrow(data.train), replace = FALSE) 
tune.train <- data.train[tuneind, ]
tune.test <- data.train[-tuneind, ]

#Set up the cross validation to select the optimal parameters
set.seed(123)
fitControl <- trainControl(method = "cv", number = 5)

# DECISION TREE (Carlotta) -----------------------------------------------------
# Change levels into Yes, No, No Vote for nicer plots
data.vote.dt <- data.vote
data.vote.dt[, 6:13] <- data.vote.dt[, 6:13] %>% mutate_if(is.numeric, as.factor)
for(i in 6:13){ data.vote.dt[, i]  <- recode(data.vote.dt[, i], 
                                          "1" = "Yes", "0" = "No Vote", "-1" = "No")
}

data.train.dt <- data.vote.dt[ind, -c(2, 4)]
data.validation.dt <- data.vote.dt[-ind,  -c(2, 4)] 

#Step1: Begin with a small cp
tree <- rpart(Constituency ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 + 
                Voting.5 + Voting.6 + Voting.7 + Voting.8, data = data.train.dt,
              control = rpart.control(cp = 0.01))

printcp(tree) #Compares trees with up to 5 splits
summary(tree)
rpart.plot(tree, type = 1, cex = 0.9)

#where cp is the complexity parameter. Any split that does not decrease the
#overall lack of fit by a factor of cp is not attempted. For instance, with
#anova splitting, this means that the overall R-squared must increase by cp at 
#each step. The main role of this parameter is to save computing time by pruning
#off splits that are obviously not worthwhile. Essentially,the user informs the
#program that any split which does not improve the fit by cp will likely be
#pruned off by cross-validation, and that hence the program need not pursue it.

#Pick the tree size that minimizes misclassification rate
#min prediction error
printcp(tree)
bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]

#Prune the tree using the best cp.
tree.pruned <- prune(tree, cp = bestcp)
rpart.plot(tree.pruned)

printcp(tree.pruned) #Uses all five nodes

#Calculate predictions based on the validation dataset
dt.pred <- predict(tree.pruned, data.validation.dt[, 4:11])


#Calculate the MSE and Accuracy/Kappa
dt.MSE<- mean((dt.pred - data.validation.dt$Constituency)^2)

dt.leave_remain_pred <- c()
for (i in 1:nrow(data.validation.dt)){
  if (dt.pred[i] >= 50) dt.leave_remain_pred[i] = "Remain"
  if (dt.pred[i] < 50) dt.leave_remain_pred[i] = "Leave"
}
dt.leave_remain_pred <- as.factor(dt.leave_remain_pred)

dt_fit_metrics <- vector("list", length(levels(data.validation.dt$LeaveRemain)))
for (i in seq_along(dt_fit_metrics)) {
  positive.class <- levels(data.validation.dt$LeaveRemain)[i]
  dt_fit_metrics[[i]] <- caret::confusionMatrix(dt.leave_remain_pred, data.validation.dt$LeaveRemain,
                                         positive = positive.class)
}

# BOOSTED TREE (Brooke) ------------------------------------------------------------
#Parameters: 
#n.trees: Integer specifying the total number of trees to fit. Default is 100.
#interaction.depth: Integer specifying the maximum depth of each tree (i.e., 
##the highest level of variable interactions allowed). A value of 1 implies an 
##additive model, a value of 2 implies a model with up to 2-way interactions, 
##etc. Default is 1.
#shrinkage: a shrinkage parameter applied to each tree in the expansion. 
##Also known as the learning rate or step-size reduction; 0.001 to 0.1 usually 
##work, but a smaller learning rate typically requires more trees. Default is 0.1.
#n.minobsinnode: Integer specifying the minimum number of observations in the 
##terminal nodes of the trees. Note that this is the actual number of observations, 
##not the total weight.
set.seed(123)
boostT_params <- train(Constituency ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 + 
                         Voting.5 + Voting.6 + Voting.7 + Voting.8, data = data.train, 
                      method = "gbm",
                      tuneGrid = expand.grid(n.trees = seq(50, 200, by = 50), 
                                             interaction.depth = seq(1, 3, by = 1), 
                                             shrinkage = seq(0.01, 0.1,by = 0.02), 
                                             n.minobsinnode = seq(2, 4, by = 1)),
                      trControl = fitControl)

#RMSE was used to select the optimal model using the smallest value.
#First wide serach: n.trees = seq(100, 500, by = 100), interaction.depth = seq(1, 5, by = 1), 
#shrinkage = seq(0.01, 0.1,by = 0.04), n.minobsinnode = seq(2, 5, by = 1)),
#n.trees = 100, interaction.depth = 2, shrinkage = 0.05 and n.minobsinnode = 2
# -> RMSE = 8.906628  R^2 = 0.3842187  MAE = 6.898437
#More refined search: n.trees = seq(50, 200, by = 50), interaction.depth = seq(1, 3, by = 1), 
#shrinkage = seq(0.01, 0.1,by = 0.02), n.minobsinnode = seq(2, 4, by = 1)),
#n.trees = 100, interaction.depth = 3, shrinkage = 0.05 and n.minobsinnode = 4.
#-> RMSE = 8.901371  R^2 = 0.3839600  MAE = 6.899207
#Final search: n.trees = seq(50, 150, by = 25), interaction.depth = seq(1, 3, by = 1), 
#shrinkage = 0.05, n.minobsinnode = seq(1, 5, by = 1))
#n.trees = 75, interaction.depth = 3, shrinkage = 0.05 and n.minobsinnode = 4.
#-> RMSE = 8.927582  R^2 = 0.3796395  MAE = 6.934375

gbm_ntrees <- boostT_params$bestTune$n.trees
gbm_depth <- boostT_params$bestTune$interaction.depth
gbm_shrink <- boostT_params$bestTune$shrinkage
gbm_mtry <- boostT_params$bestTune$n.minobsinnode

gbm_model <- gbm(Constituency ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 + 
                   Voting.5 + Voting.6 + Voting.7 + Voting.8, data = data.train,
                     n.trees = gbm_ntrees,
                     interaction.depth = gbm_depth,
                     shrinkage = gbm_shrink,
                     n.minobsinnode = gbm_mtry, distribution = "gaussian")

#Make predictions
gbm.pred <- predict(gbm_model, n.trees = gbm_ntrees, newdata = data.validation[, 4:11], type = "response") 
gbm.MSE <- mean((gbm.pred  - data.validation$Constituency)^2)

#Change probabilities into classes
gbm.leave_remain_pred <- c()
for (i in 1:nrow(data.validation)){
  if (gbm.pred[i] >= 50) gbm.leave_remain_pred[i] = "Remain"
  if (gbm.pred[i] < 50) gbm.leave_remain_pred[i] = "Leave"
}
gbm.leave_remain_pred <- as.factor(gbm.leave_remain_pred)

gbm_fit_metrics <- vector("list", length(levels(data.validation$LeaveRemain)))
for (i in seq_along(gbm_fit_metrics)) {
  positive.class <- levels(data.validation$LeaveRemain)[i]
  gbm_fit_metrics[[i]] <- caret::confusionMatrix(gbm.leave_remain_pred, data.validation$LeaveRemain,
                                         positive = positive.class)
}


# gbm_ntrees <- boostT_params$bestTune$n.trees
# gbm_depth <- boostT_params$bestTune$interaction.depth
# gbm_shrink <- boostT_params$bestTune$shrinkage
# gbm_mtry <- boostT_params$bestTune$n.minobsinnode
# 
# gbm_model <- gbm(factor(Party) ~ ., data = data.train,
#                      n.trees = gbm_ntrees,
#                      interaction.depth = gbm_depth,
#                      shrinkage = gbm_shrink,
#                      n.minobsinnode = gbm_mtry, distribution = "multinomial")

# RANDOM FOREST (Lei) ----------------------------------------------------------
source("Scripts/RFFunction.R")
set.seed(123)
### create model for percentage prediction, regression random forest ###
originalreg.rf = randomForest(Constituency ~ Voting.1 + Voting.2 + Voting.3 + 
                                Voting.4 + Voting.5 + Voting.6 + Voting.7 + 
                                Voting.8, data = tune.train,
                              mtry = 2, ntree = 1000)
print(originalreg.rf)
plot(originalreg.rf)

### Try to find out the best mtry value
findgoodmtryreg(tune.train[, 4:11], tune.train[, 3], 10, 123, tune.test)
### when mtry = 3, the MSE is minimum

## creat the new model, mtry = 3 and try to find the best ntree
modelreg.lr <- randomForest(Constituency ~ Voting.1 + Voting.2 + Voting.3 + 
                              Voting.4 + Voting.5 + Voting.6 + Voting.7 + 
                              Voting.8, mtry = 3, data = tune.train, ntree = 1100)
print(modelreg.lr)
plot(modelreg.lr)

## from the plot, we could know when ntree around 800 - 1000 is stable.
findgoodntreereg(tune.train[, 4:11], tune.train[, 3], 800, 1200, 50, 3, tune.test)
findgoodntreereg(tune.train[, 4:11], tune.train[, 3], 900, 1100, 30, 3, tune.test)
findgoodntreereg(tune.train[, 4:11], tune.train[, 3], 900, 1100, 20, 3, tune.test)
findgoodntreereg(tune.train[, 4:11], tune.train[, 3], 900, 1100, 10, 3, tune.test)
## when ntree = 1000, mtry = 3, the MSE is minimum

## creat the best model of random forest for regression
modeltworeg.lr <- randomForest(Constituency ~ Voting.1 + Voting.2 + Voting.3 +
                                 Voting.4 + Voting.5 + Voting.6 + Voting.7 + 
                                 Voting.8, data = data.train, 
                               mtry = 3, ntree = 1000)
print(modeltworeg.lr)
plot(modeltworeg.lr)

#Obtain predicitions based on validation set
rf.pred <- predict(modeltworeg.lr, data.validation)

#Calculate the MSE and Accuracy/Kappa
rf.MSE <- mean((rf.pred - data.validation$Constituency)^2)

rf.leave_remain_pred <- c()
for (i in 1:nrow(data.validation)){
  if (rf.pred[i] >= 50) rf.leave_remain_pred[i] = "Remain"
  if (rf.pred[i] < 50) rf.leave_remain_pred[i] = "Leave"
}
rf.leave_remain_pred <- as.factor(rf.leave_remain_pred)

rf_fit_metrics <- vector("list", length(levels(data.validation$LeaveRemain)))
for (i in seq_along(rf_fit_metrics)) {
  positive.class <- levels(data.validation$LeaveRemain)[i]
  rf_fit_metrics[[i]] <- caret::confusionMatrix(rf.leave_remain_pred, data.validation$LeaveRemain,
                                         positive = positive.class)
}

# GLM (Abtin) ------------------------------------------------------------------
#Building an initial glm model
logit <- glm(formula <- Constituency ~ . -Party -LeaveRemain, data = data.train)
summary(logit)
#AIC: 3263.9

car::Anova(logit)
#at the 5% level keep only Voting.3, Voting.5, Voting.6 and Voting.7

step(logit, direction = "both")

logit <- glm(formula = Constituency ~ Voting.3 + Voting.4 + Voting.5 + 
             Voting.6 + Voting.7, data = data.train)

summary(logit)

# predict <- predict(logit, tune.test, type = 'response')
# glm.MSE <- mean((predict - tune.test$Constituency)^2)   #89.54375
#Not sure if getting rid of the votes helped - fit actually gets worse.

#Trying cross validation
set.seed(123)
shuffled_indices <- sample(1:nrow(data.train))
shuffledData <- data.train[shuffled_indices, ]
cv_folds <- cut(seq(1, nrow(shuffledData)), breaks = 5, labels = FALSE)

#Create a store for cv_scores
mse_scores <- rep(NA, 5)

#Perform 5 fold cross validation
for(j in 1:5){
  #Segement your data by fold using the which() function 
  cv_indices <- which(cv_folds == j, arr.ind = TRUE)
  cvTest <- data.train[cv_indices, ]
  cvTrain <- data.train[-cv_indices, ]
  
  #Build the polynomial and store cv scores
  glm_cv <- glm(formula = Constituency ~ Voting.3 + Voting.4 + Voting.5 + 
                  Voting.6 + Voting.7, data = cvTrain)
  
  #Estimate mse
  mse_scores[j] <- mean((cvTest$Constituency - predict(glm_cv, cvTest))^2) 
}

#Take the mean of the goodness of fit measures
glm.cv.MSE <- mean(mse_scores)  #83.55
#Now test on Validation set.
glm.pred <- predict(logit, data.validation, type = 'response')

#Calculate the MSE and Accuracy/Kappa
glm.MSE <- mean((glm.pred - data.validation$Constituency)^2)

glm.leave_remain_pred <- c()
for (i in 1:nrow(data.validation)){
  if (glm.pred[i] >= 50) glm.leave_remain_pred[i] = "Remain"
  if (glm.pred[i] < 50) glm.leave_remain_pred[i] = "Leave"
}
glm.leave_remain_pred <- as.factor(glm.leave_remain_pred)

glm_fit_metrics <- vector("list", length(levels(data.validation$LeaveRemain)))
for (i in seq_along(glm_fit_metrics)) {
  positive.class <- levels(data.validation$LeaveRemain)[i]
  glm_fit_metrics[[i]] <- caret::confusionMatrix(glm.leave_remain_pred, data.validation$LeaveRemain,
                                          positive = positive.class)
}
# SUPPORT VECTOR MACHINE (José) ------------------------------------------------
svmfit <- svm(Constituency ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 +
                Voting.5 + Voting.6 + Voting.7 + Voting.8, data = data.train,
              kernel = "linear", cost = 10, scale = FALSE)

tune.out.svm <- tune(svm, Constituency ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 +
                       Voting.5 + Voting.6 + Voting.7 + Voting.8, 
                     data = data.train, kernel = "linear",
                     ranges =list(cost=c(0.1,1,10,100),gamma=c(0.5,1,2,3,4)))
summary(tune.out.svm)

svmfit <- svm(Constituency ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 +
                Voting.5 + Voting.6 + Voting.7 + Voting.8, data = data.train,
              kernel = "linear", cost = 0.1, gamma = 0.5,  scale = FALSE)

#Calculate predictions based on validation set
svm.pred <- predict(svmfit, data.validation[, 4:11])

#Calculate the MSE and accuracy
svm.MSE <- mean((svm.pred - data.validation$Constituency)^2)

leave_remain_pred <- c()
for (i in 1:nrow(data.validation)){
  if (svm.pred[i] >= 50) leave_remain_pred[i] = "Remain"
  if (svm.pred[i] < 50) leave_remain_pred[i] = "Leave"
}

leave_remain_pred <- as.factor(leave_remain_pred)

svm_fit_metrics <- vector("list", length(levels(data.validation$LeaveRemain)))
for (i in seq_along(svm_fit_metrics)) {
  positive.class <- levels(data.validation$LeaveRemain)[i]
  svm_fit_metrics[[i]] <- caret::confusionMatrix(leave_remain_pred, data.validation$LeaveRemain,
                                          positive = positive.class)
}

svm.leave_remain_pred <- leave_remain_pred

# NEURAL NET (Brooke) ----------------------------------------------------------
set.seed(123)
nn_params <- train(Constituency ~ Voting.1 + Voting.2 + Voting.3 + 
                     Voting.4 + Voting.5 + Voting.6 + Voting.7 + 
                     Voting.8, data = data.train, 
                   method = "nnet", linear.output = TRUE,
                   trControl = fitControl)
nn_hidden <- nn_params$bestTune$size #Suggests 1 hidden layer
nn_decay <- nn_params$bestTune$decay

nn_model <- neuralnet(Constituency ~ Voting.1 + Voting.2 + Voting.3 + 
                        Voting.4 + Voting.5 + Voting.6 + Voting.7 + 
                        Voting.8, data = data.train,
                      stepmax = 1e+06, 
                      linear.output = TRUE, hidden = nn_hidden,
                      lifesign = "full")

plot(nn_model)

#Make predictions
nn.pred  <- predict(nn_model, data.validation[, 4:11])
nn.MSE <- mean((nn.pred  - data.validation$Constituency)^2)

#Change probabilities into classes
nn.leave_remain_pred <- c()
for (i in 1:nrow(data.validation)){
  if (nn.pred[i] >= 50) nn.leave_remain_pred[i] = "Remain"
  if (nn.pred[i] < 50) nn.leave_remain_pred[i] = "Leave"
}
nn.leave_remain_pred <- as.factor(nn.leave_remain_pred)

nn_fit_metrics <- vector("list", length(levels(data.validation$LeaveRemain)))
for (i in seq_along(nn_fit_metrics)) {
  positive.class <- levels(data.validation$LeaveRemain)[i]
  nn_fit_metrics[[i]] <- caret::confusionMatrix(nn.leave_remain_pred, data.validation$LeaveRemain,
                                         positive = positive.class)
}

# COMPARING MODEL ACCURACY (Brooke) --------------------------------------------
#Obtain Kappas
dt_kappa <- dt_fit_metrics[[1]]$overall["Kappa"]
gbm_kappa <- gbm_fit_metrics[[1]]$overall["Kappa"]
rf_kappa <- rf_fit_metrics[[1]]$overall["Kappa"]
glm_kappa <- glm_fit_metrics[[1]]$overall["Kappa"]
svm_kappa <- svm_fit_metrics[[1]]$overall["Kappa"] #Appears to be the best kappa?
nn_kappa <- nn_fit_metrics[[1]]$overall["Kappa"]

dt_acc <- dt_fit_metrics[[1]]$overall["Accuracy"]
gbm_acc <- gbm_fit_metrics[[1]]$overall["Accuracy"]
rf_acc <- rf_fit_metrics[[1]]$overall["Accuracy"]
glm_acc <- glm_fit_metrics[[1]]$overall["Accuracy"]
svm_acc <- svm_fit_metrics[[1]]$overall["Accuracy"] #Appears to be the best kappa?
nn_acc <- nn_fit_metrics[[1]]$overall["Accuracy"]

kappas <- round(c(DT = dt_kappa, GBM = gbm_kappa, RF = rf_kappa, GLM = glm_kappa, SVM = svm_kappa, NN = nn_kappa), 4)
accs <- round(c(DT = dt_acc, GBM = gbm_acc, RF = rf_acc, GLM = glm_acc, SVM = svm_acc, NN = nn_acc), 4)
mse <- round(c(DT = dt.MSE, GBM = gbm.MSE, RF = rf.MSE, GLM = glm.MSE, SVM = svm.MSE, NN = nn.MSE), 4)

comparison <- cbind(MSE = mse, ACC = accs, KAPPA = kappas)
comparison

# #ROC Curve
# 
# dt.roc <- plot(roc(data.validation$LeaveRemain, order(dt.leave_remain_pred)),
#                col="red", lwd = 2)
# gbm.roc <- plot(roc(data.validation$LeaveRemain, order(gbm.leave_remain_pred)),
#                col="black", lwd = 2, add = TRUE)
# rf.roc <- plot(roc(data.validation$LeaveRemain, order(rf.leave_remain_pred)),
#                lwd = 2, col = "purple", add = TRUE)
# glm.roc <- plot(roc(data.validation$LeaveRemain, order(glm.leave_remain_pred)),
#                 lwd = 2, col = "yellow", add = TRUE)
# svm.roc <- plot(roc(data.validation$LeaveRemain, order(svm.leave_remain_pred)),
#                 lwd = 2, col = "blue", add = TRUE)
# nn.roc <- plot(roc(data.validation$LeaveRemain, order(nn.leave_remain_pred)),
#                lwd = 2, col = "green", add = TRUE)
# legend("bottomright", 
#        legend = c("Decision Tree (AUC = 0.725/0.635)",
#                   "Gradient Boosted Tree (AUC = 0.734/0.624)", 
#                   "Random Forest (AUC = 0.703/0.612)", 
#                   "Generalized Linear Model (AUC = 0.728/0.639)", 
#                   "Support Vector Machine (AUC = 0.731/0.653)", 
#                   "Neural Net (AUC = 0.731/0.613)"), cex = 0.75,
#        col = c("red", "purple", "yellow", "blue", "green"), lwd = 2)


# plotwithggplot <- ggplot(data.frame(roc(data.validation$LeaveRemain, order(dt.leave_remain_pred), 
#                                         aes(FPR, TPR)))) + geom_point() +
#                                       geom_abline(intercept = 0, slope = 1)


par(mfrow = c(1,1))
roc.curve(data.validation$LeaveRemain, dt.leave_remain_pred, plotit = TRUE, add.roc = FALSE, col = "red")
roc.curve(data.validation$LeaveRemain, gbm.leave_remain_pred, plotit = TRUE, add.roc = TRUE, col = "black")
roc.curve(data.validation$LeaveRemain, rf.leave_remain_pred, plotit = TRUE, add.roc = TRUE, col = "purple")
roc.curve(data.validation$LeaveRemain, glm.leave_remain_pred, plotit = TRUE, add.roc = TRUE, col = "yellow")
roc.curve(data.validation$LeaveRemain, svm.leave_remain_pred, plotit = TRUE, add.roc = TRUE, col = "blue")
roc.curve(data.validation$LeaveRemain, nn.leave_remain_pred, plotit = TRUE, add.roc = TRUE, col = "green")
legend("bottomright", 
       legend = c("Decision Tree (AUC = 0.725)",
                  "Gradient Boosted Tree (AUC = 0.734)", 
                  "Random Forest (AUC = 0.703)", 
                  "Generalized Linear Model (AUC = 0.728)", 
                  "Support Vector Machine (AUC = 0.731)", 
                  "Neural Net (AUC = 0.731)"), cex = 0.75,
       col = c("red", "purple", "yellow", "blue", "green"), lwd = 2)

# par(mfrow = c(1, 2))
# #Plot Sensitivities
# dt_Sens <- c(dt_fit_metrics[[1]]$byClass["Sensitivity"], dt_fit_metrics[[2]]$byClass["Sensitivity"])
# rf_Sens <- c(rf_fit_metrics[[1]]$byClass["Sensitivity"], rf_fit_metrics[[2]]$byClass["Sensitivity"])
# glm_Sens <- c(glm_fit_metrics[[1]]$byClass["Sensitivity"], glm_fit_metrics[[2]]$byClass["Sensitivity"])
# svm_Sens <- c(svm_fit_metrics[[1]]$byClass["Sensitivity"], svm_fit_metrics[[2]]$byClass["Sensitivity"])
# nn_Sens <- c(svm_fit_metrics[[1]]$byClass["Sensitivity"], svm_fit_metrics[[2]]$byClass["Sensitivity"])
# 
# plot(x = 1:2, y = dt_Sens, ylim = c(0, 1), xlab = "Party", ylab = "Sensitivity",
#      main = "Sensitivity Comparison Based on Validation Set",
#      type = "b", col = "red", xaxt = "n")
# axis(1, at = c(1, 2), labels = c("Leave", "Remain"))
# points(x = 1:2, y = rf_Sens, type = "b", col = "purple", lty = 2)
# points(x = 1:2, y = glm_Sens, type = "b", col = "yellow")
# points(x = 1:2, y = svm_Sens, type = "b", col = "blue")
# points(x = 1:2, y = nn_Sens, type = "b", col = "green", lty = 2)
# legend("bottomleft", legend = c("Classification Tree",
#                                 "Random Forest",
#                                 "Generalised Linear Model",
#                                 "Support Vector Machine",
#                                 "Neural Net"),
#        col = c("red", "purple", "yellow", "blue", "green"),
#        lwd = 2, cex = 0.75)
# 
# #Plot Specificity
# dt_Spec <- c(dt_fit_metrics[[1]]$byClass["Specificity"], dt_fit_metrics[[2]]$byClass["Specificity"])
# rf_Spec <- c(rf_fit_metrics[[1]]$byClass["Specificity"], rf_fit_metrics[[2]]$byClass["Specificity"])
# glm_Spec <- c(glm_fit_metrics[[1]]$byClass["Specificity"], glm_fit_metrics[[2]]$byClass["Specificity"])
# svm_Spec <- c(svm_fit_metrics[[1]]$byClass["Specificity"], svm_fit_metrics[[2]]$byClass["Specificity"])
# nn_Spec <- c(svm_fit_metrics[[1]]$byClass["Specificity"], svm_fit_metrics[[2]]$byClass["Specificity"])
# 
# plot(x = 1:2, y = dt_Spec, ylim = c(0, 1), xlab = "Party", ylab = "Specificity",
#      main = "Specificity Comparison Based on Validation Set", lty = 2,
#      type = "b", col = "red", xaxt = "n")
# axis(1, at = seq(1, 4, by = 1), labels = c("Conservative", "Labour",
#                                            "Other", "Scottish\nNational Party"))
# points(x = 1:2, y = rf_Spec, type = "b", col = "purple", lty = 2)
# points(x = 1:2, y = glm_Spec, type = "b", col = "yellow", lty = 2)
# points(x = 1:2, y = svm_Spec, type = "b", col = "blue", lty = 2)
# points(x = 1:2, y = nn_Spec, type = "b", col = "green", lty = 2)
# legend("bottomleft", legend = c("Classification Tree",
#                                 "Random Forest",
#                                 "Logistic Regression",
#                                 "Support Vector Machine",
#                                 "Neural Net"),
#        col = c("red", "purple", "yellow", "blue", "green"),
#        lwd = 2, cex = 0.75)

#Aim 1: Gradient boost (97% acc) and then random forest (96%)
#Aim 2: Decision tree [Gradient boost (75% acc)]

#nb
# nn
# rf
# dt
# xj boost (some sort of dt)
