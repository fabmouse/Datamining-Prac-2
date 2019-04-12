voteData <- read.csv("Data/Clean Vote Data", header = True)
attach(voteData)
library(tree)

#create decision tree matrix
tree.voting <- tree(Party ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 +
                      Vote.5 + Vote.6 + Vote.7 + Vote.8, data = voteData)

plot(tree.voting)
text(tree.voting, pretty = 0)

set.seed(100)
train<-sample(1:nrow(voteData), 100)
tree.voting1 <- tree(Party ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 +
                       Vote.5 + Vote.6 + Vote.7 + Vote.8, data = voteData, subset = train)
plot(tree.voting1)
text(tree.voting1, pretty = 0)

tree.predict.train <- predict(tree.voting1, voteData[-train,], type = "class")
with(voteData[-train], table(tree.predict, Party))
tree.predict

tree.predict <- predict(tree.voting1, voteData, type = "class")
with(voteData, table(tree.predict, Party))

cv.tree(tree.predict)

bla <- data.frame(tree.predict, voteData$Party)
if bla$tree.predict != bla$voteData.Party,
then 

(276+226+34)/(276+4+27+2+226+5+2+13+13+34)#0.89




