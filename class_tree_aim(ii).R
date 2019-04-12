#### classifiation tree - aim (ii) ####

dat <- remainORleave
library(rpart)
library(rpart.plot)

#Step1: Begin with a small cp
set.seed(123)
tree <- rpart(LeaveRemain ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 + 
                Voting.5 + Voting.6 + Voting.7 + Voting.8, data = dat1, method = "class"
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

conf.matrix <- table(dat1$LeaveRemain[!is.na(dat1$LeaveRemain)], predict(tree.pruned,type="class"))
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
dat <- Final_Downloaded_Data
as.numeric(dat$Constituency)
dat <- na.omit(dat)
library(rpart)
library(rpart.plot)

#Step1: Begin with a small cp
set.seed(123)
tree <- rpart(Constituency ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 + 
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

#
pred_reg <- predict(tree.pruned,type="class")
pred_reg <- as.numeric(pred_reg)

#plots [doesn't make sense since continuous]
#plot(tree.pruned); text(tree.pruned, cex = 0.8, use.n = TRUE, xpd = TRUE)


#### MSE ####

dat$Constituency <- as.numeric(dat$Constituency)
dat$Constituency <- as.numeric(dat$Constituency)

diff <- pred_reg - dat$Constituency

MSE <- sum(diff^2)
