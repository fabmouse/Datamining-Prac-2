###################
#NEURAL NET SCRIPT#
###################

library(neuralnet)

data.vote <- read.csv("data/Clean Vote Data.csv", header = TRUE)
head(data.vote)

# estimate a neural network with one hidden layer of 8 nodes
nn <- neuralnet(as.factor(Party) ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + 
                  Vote.5 + Vote.6 + Vote.7 + Vote.8, data = data.vote,
                linear.output = FALSE, hidden = 1, lifesign="full")
plot(nn)

# do the predictions of the neural network look good?
nn.dat <- prediction(nn)$data
