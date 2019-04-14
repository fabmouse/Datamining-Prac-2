library(e1071)

set.seed(30)

voteData <- read.csv("data/remainORleave.csv", header=T)
attach(voteData)
head(voteData)


train <- sample(1:nrow(voteData), 100)
voteData.test <- voteData[-train,]
Constituency.test <- Constituency[-train]

svmfit <- svm(Constituency ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 +
                Voting.5 + Voting.6 + Voting.7 + Voting.8, voteData[train,],
              kernel = "radial", gamma = 1, cost = 1)
plot(svmfit, voteData)


svmfit <- svm(Constituency ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 +
                Voting.5 + Voting.6 + Voting.7 + Voting.8, voteData.test,
              kernel = "linear", cost = 10, scale = FALSE)

set.seed(35)

tune.out.svm <- tune(svm, Constituency ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 +
                        Voting.5 + Voting.6 + Voting.7 + Voting.8, 
                     data = voteData[train,], kernel = "radial",
                     ranges =list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out.svm)
table(true=voteData[-train, "Constituency"], pred=predict(tune.out.svm$best.model, newdata = voteData[-train,]))

