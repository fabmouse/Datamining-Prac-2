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
voteData <- read.csv("Data/Downloaded Data.csv")
head(voteData)

# DATA CLEANING -----------------------------------------------------------

#1. Change the coding of the votes
levels(voteData$Voting.1)
#Votes are factor variables with 3 levels: "Aye", "No", and "No Vote Recorded"
#Recode "gv-against\"" as -1, "gv-did-not-vote\"" as 0  and "gv-for\"" as 1, 

for(i in 4:ncol(voteData)){
  levels(voteData[, i]) <- c(-1, 0, 1)
}
head(voteData)
glimpse(voteData)
levels(voteData$Vote.1)

#2. Remove rows with all NA values

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

voteClean$Party <- recode(voteClean$Party, 
                          "Con\n" = "CON",
                          "Lab\n" = "LAB",
                          "SNP\n" = "SNP",
                          "DUP\n"= "OTH", 
                          "Grn\n" = "OTH", 
                          "Ind\n" = "OTH", 
                          "LD\n" = "OTH", 
                          "Oth\n" = "OTH", 
                          "PC\n" = "OTH", 
                          "SF\n" = "OTH", 
                          "TIG\n" = "OTH")
table(voteClean$Party) 

write.csv(voteClean, "Data/Clean Vote Data.csv", row.names = FALSE)

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


