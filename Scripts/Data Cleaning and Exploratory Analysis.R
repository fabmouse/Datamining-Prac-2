##################################
#EXPLORATORY DATA ANALYSIS SCRIPT#
##################################

#Project Aims: use an MP's vote profile to predict
# (i) the party they represent
# (ii) the percentage leave/remain vote for their constituency in the Brexit 
# referendum.

# LIBRARIES ---------------------------------------------------------------
library(dplyr)

# IMPORT THE DATASET ------------------------------------------------------
voteData <- read.csv("Data/data combined.csv")
head(voteData)

# DATA CLEANING -----------------------------------------------------------

#1. Change the coding of the votes
levels(voteData$Vote.1)
#Votes are factor variables with 3 levels: "Aye", "No", and "No Vote Recorded"
#Recode "Aye" as 1, "No" as 0 and "No Vote Recorded" as NA

# for(i in 4:ncol(voteData)){
#   levels(voteData[, i]) <- c(1, 0, NA)
# }
# head(voteData)
# glimpse(voteData)
# levels(voteData$Vote.1)

#Recode "Aye" as 1, "No" as -1 and "No Vote Recorded" as 0

for(i in 4:ncol(voteData)){
  levels(voteData[, i]) <- c(1, -1, 0)
}
head(voteData)
glimpse(voteData)
levels(voteData$Vote.1)

# voteData[, 4:11] <- apply(voteData[, 4:11], MARGIN = 2, 
#                           FUN = recode, "Aye" = 1, "No" = 0)
#8 warnings were produced, but I ignored them as it simply replaced "No Vote" as 
#NA

#2. Remove rows with all NA values
#There are 47 instances where the Cabinet members and SF politicians abstained 
#throughout
# 
# abstained <- voteData[, 4:11] %>%
#                       is.na() %>%
#                       apply(MARGIN = 1, FUN = all)
# ab_ind <- which(abstained == TRUE)


ab_ind <- vector()

for(i in 1:nrow(voteData)){
  if(all(voteData[i, 4:11] == 0)) {
    ab_ind <- c(ab_ind, i)
    }
}

voteClean <- voteData[- ab_ind, ]

#Note, there are still 662 missing values
#If we delete observations where there any number of missing values then we will
#be left with only 281 observations.
#sum(is.na(voteClean))
#voteComplete <- na.omit(voteData)

#3. Group all small parties
table(voteClean$Party) 

# The largest were Conservative = 280, Labour = 243, Scottish National Party = 34
# Combine the remaining parties as "Other"

voteClean$Party <- recode(voteClean$Party, "Democratic Unionist Party"= "Other", 
                       "Deputy Speaker" = "Other", 
                       "Green Party" = "Other", 
                       "Independent" = "Other", 
                       "Liberal Democrat" = "Other", 
                       "Plaid Cymru" = "Other", 
                       "Sinn F?in" = "Other", 
                       "Speaker" = "Other")
table(voteClean$Party) 

#Fill NA with missing vote by majority vote of each party or random 0/1 if 
#majority is NA

# voteClean <- arrange(voteClean, Party)
# #1 - 280: Conservative
# #281 - 325: Other 
# #326 - 568: Labour
# #569 - 602: SNP
# 
# c <- c(0, 280, 325, 568, 602)
# for ( i in 1:(length(c) - 1)){ # for every party
#   for (j in 4:11) { #for every vote
#       
#       zeros <- length(which(voteClean[(c[i] + 1 : c[i + 1]), j] == 0))
#       ones <- length(which(voteClean[(c[i] + 1 : c[i + 1]),j] == 1))
#       Nas <- length(which(is.na(voteClean[(c[i] + 1 : c[i + 1]),j])))
#       max <- which.is.max(c(zeros, ones, Nas))
#       
#       set.seed(123)
#       if(max == 1) voteClean[,j][is.na(voteClean[,j])] <- 0
#       if(max == 2) voteClean[,j][is.na(voteClean[,j])] <- 1
#       if(max == 3) voteClean[,j][is.na(voteClean[,j])] <- sample(c(0,1))
#       
#     }
# }
# 
# sum(is.na(voteClean)) #YAY


# EXPLORATORY ANALYSIS ----------------------------------------------------
glimpse(voteClean)
summary(voteClean)

# #Visualise observed votes
# library(ggplot2)
# library(scales)
# 
# df <- expand.grid(factor(voteClean$Party),
#                  factor(voteClean[, 4:11]),
#                  factor(c(1, 0)))
#             
# ggplot(data = df, aes(x = voteClean[, 4:11], y = Value, fill = Vote.1)) + 
#   geom_bar(stat='identity') + 
#   facet_wrap(~voteClean$Party) +
#   #scale_y_continuous(labels = percent) +
#   theme(panel.background = element_rect(fill = "white"))   

# AIM ONE: PREDICT PARTY --------------------------------------------------


# AIM TWO: PREDICT LEAVE/REMAIN PERCENTAGE --------------------------------


