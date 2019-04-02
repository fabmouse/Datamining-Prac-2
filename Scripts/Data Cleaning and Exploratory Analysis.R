##################################
#EXPLORATORY DATA ANALYSIS SCRIPT#
##################################

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

for(i in 4:ncol(voteData)){
  levels(voteData[, i]) <- c(1, 0, NA)
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

abstained <- voteData[, 4:11] %>%
                      is.na() %>%
                      apply(MARGIN = 1, FUN = all)
ab_ind <- which(abstained == TRUE)
  
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

# EXPLORATORY ANALYSIS ----------------------------------------------------

