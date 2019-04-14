#### classifiation tree - aim (ii) ####


#  previous working -------------------------------------------------------
dat <- remainORleave
library(rpart)
library(rpart.plot)

dat$Constituency <- as.numeric(dat$Constituency)

#Step1: Begin with a small cp
set.seed(123)
tree <- rpart(LeaveRemain ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 + 
                Voting.5 + Voting.6 + Voting.7 + Voting.8, data = dat, method = "class"
              , control = rpart.control(cp = 0.00000001))#split: 0, 1, 4
printcp(tree)
plotcp(tree)

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

printcp(tree.pruned)

# confusion matrix (training data)
predict(tree.pruned,type="class")

conf.matrix <- table(dat$LeaveRemain[!is.na(dat$LeaveRemain)], predict(tree.pruned,type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
conf.matrix

#plots
plot(tree.pruned); text(tree.pruned, cex = 0.8, use.n = TRUE, xpd = TRUE)

#better looking
prp(tree.pruned, faclen = 0, cex = 0.8, extra = 1)

#better looking
x11()
tot_count <- function(x, labs, digits, varlen)
{
  paste(labs, "\n\nn =", x$frame$n)
}

prp(tree.pruned, faclen = 0, cex = 0.8, node.fun=tot_count)

#better looking
only_count <- function(x, labs, digits, varlen)
{
  paste(x$frame$n)
  paste(labs, "\n\nn =", x$frame$n)
}

boxcols <- c("indianred1", "palegreen3")[tree.pruned$frame$yval]

par(xpd=TRUE)
prp(tree.pruned, faclen = 0, cex = 0.8, node.fun=only_count, box.col = boxcols)
legend("topleft", legend = c("Leave","Remain"), fill = c("indianred1", "palegreen3"),
       title = "Constituency")


##################################
#### decision tree - aim (ii) ####
##################################
# dat <- Final_Downloaded_Data
# dat <- read.csv("data/Final Downloaded Data.csv")
# dat <- breixtdata
# dat <- na.omit(dat)
# dat$Constituency <- as.numeric(dat$Constituency)

data.vote <- read.csv("data/Final Vote Data.csv", header = TRUE)
data.vote <- na.omit(data.vote)
data.vote$Constituency <- as.numeric(data.vote$Constituency)
data.vote[, 6:13] <- data.vote[, 6:13] %>% 
  mutate_if(is.numeric, as.factor)

for(i in 6:13){ data.vote[, i]  <- recode(data.vote[, i], 
                               "1" = "Yes", "0" = "No Vote", "-1" = "No")
}


#Split dataset into a training set (75%) and hold back (25%) for validation
set.seed(123)
ind <- sample(1:nrow(data.vote), 0.75 * nrow(data.vote), replace = FALSE) 
data.train <- data.vote[ind, -c(2, 4)]
data.validation <- data.vote[-ind,  -c(2, 4)] 

library(rpart)
library(rpart.plot)

#Step1: Begin with a small cp
set.seed(123)
tree <- rpart(Constituency ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 + 
                Voting.5 + Voting.6 + Voting.7 + Voting.8, data = data.train,
               control = rpart.control(cp = 0.01)) #split: 0, 1, 4

printcp(tree)
summary(tree)
rpart.plot(tree, type = 1)

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

printcp(tree.pruned)

#
pred_reg <- predict(tree.pruned, data.validation[, 4:11])
#pred_reg <- as.numeric(pred_reg)

#plots [doesn't make sense since continuous]
#plot(tree.pruned); text(tree.pruned, cex = 0.8, use.n = TRUE, xpd = TRUE)


#### MSE ####

diff <- pred_reg - data.validation$Constituency

MSE <- mean(diff^2)

#transform the percentage with Leave or Remain 
#50 - 100 is Remain 
#0 - 49 is Leave

leave_remain_pred <- c()
for (i in 1:nrow(data.validation)){
  if (pred_reg[i] >= 50) leave_remain_pred[i] = "Remain"
  if (pred_reg[i] < 50) leave_remain_pred[i] = "Leave"
}
leave_remain_pred <- as.factor(leave_remain_pred)

table(leave_remain_pred)
table(observed = data.validation$LeaveRemain, predicted = leave_remain_pred)

dt_fit_metrics <- vector("list", length(levels(data.validation$LeaveRemain)))
for (i in seq_along(dt_fit_metrics)) {
  positive.class <- levels(data.validation$LeaveRemain)[i]
  dt_fit_metrics[[i]] <- confusionMatrix(leave_remain_pred, data.validation$LeaveRemain,
                                         positive = positive.class)
}


