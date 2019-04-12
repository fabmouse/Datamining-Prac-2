set.seed(11)
train <- sample(1:nrow(voteData), 200)
attach(voteData)
svmfit <- svm(Party ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + Vote.5 + Vote.6 +
                Vote.7 + Vote.8, data = voteData[train,], kernel = "radial", gamma = 1,
              cost = 1)
plot(svmfit, voteData[train,])
svmfit2 <- svm(Party ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + Vote.5 + Vote.6 +
                Vote.7 + Vote.8, data = voteData[train,], kernel = "radial", gamma = 1,
              cost = 1e5)
tune.out <- tune(svm, Party ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + Vote.5 + Vote.6 +
                   Vote.7 + Vote.8, data = voteData[train,], kernel = "radial", 
                 ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
tune.out$best.model

table(true = voteData[-train, "y"], pred = predict(tune.out$best.model, newdata = voteData[-train,]))
