library(e1071)

data.vote <- read.csv("data/Final Vote Data.csv", header = TRUE)
data.vote <- na.omit(data.vote)
data.vote$Constituency <- as.numeric(data.vote$Constituency)
head(data.vote)

#Split dataset into a training set (75%) and hold back (25%) for validation
set.seed(123)
ind <- sample(1:nrow(data.vote), 0.75 * nrow(data.vote), replace = FALSE) 
data.train <- data.vote[ind, -c(2, 4)]
data.validation <- data.vote[-ind,  -c(2, 4)] 


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

pred_reg <- predict(svmfit, data.validation[, 4:11])

#### MSE ####
diff <- pred_reg - data.validation$Constituency

MSE <- mean(diff^2)

leave_remain_pred <- c()
for (i in 1:nrow(data.validation)){
  if (pred_reg[i] >= 50) leave_remain_pred[i] = "Remain"
  if (pred_reg[i] < 50) leave_remain_pred[i] = "Leave"
}
leave_remain_pred <- as.factor(leave_remain_pred)

table(leave_remain_pred)
table(observed = data.validation$LeaveRemain, predicted = leave_remain_pred)

svm_fit_metrics <- vector("list", length(levels(data.validation$LeaveRemain)))
for (i in seq_along(svm_fit_metrics)) {
  positive.class <- levels(data.validation$LeaveRemain)[i]
  svm_fit_metrics[[i]] <- confusionMatrix(leave_remain_pred, data.validation$LeaveRemain,
                                         positive = positive.class)
}
svm_fit_metrics



