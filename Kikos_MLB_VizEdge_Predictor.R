###Alex Kikos - Fall 2021

#clear working directory
rm(list=ls())

##libraries
library(readxl)
library(sandwich)
library(corrplot)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(caret)



###DATA MANAGEMENT/TRANSFORMATIONS###
###DATA MANAGEMENT/TRANSFORMATIONS###
###DATA MANAGEMENT/TRANSFORMATIONS###
###DATA MANAGEMENT/TRANSFORMATIONS###



#reads in both sheets of Vizual Edge raw data, Sheet 1 = numerical values, Sheet 2 = textual values
df1<-read_excel("./VizEdgeData_Final.xlsx", sheet = "Sheet1")
df2<-read_excel("./VizEdgeData_Final.xlsx", sheet = "Sheet2")

#finds the max (best) values for ten Vizual Edge variables, based on UID
maxdata<- aggregate(cbind(Edge,Depth,Convergence,Divergence,Recognition1,Tracking1,DraftYear,DraftAge,DraftRound,DraftPick) ~ UID, df1, max)
#finds the min (best) values for three Vizual Edge variables, based on UID
mindata<- aggregate(cbind(Alignment,Recognition2,Tracking2) ~ UID, df1, min)
#removes duplicates of textual data - based on UID
textdata<- distinct(df2, UID,LevelReached,DraftType,Status)

#merges the separate variable sets into one clean data set based on UID
myData<-merge (maxdata,mindata,by="UID")
myData<-merge (myData,textdata,by="UID")

#Creates Dummy Variables - transforms categorical data into numerical values (1s & 0s for future logistical reg use)
myData$ReachedMLB<-ifelse(myData$LevelReached == "MLB",1,0)
myData$CollegePlayer<-ifelse(myData$DraftType == "College",1,0)
myData$StillActive<-ifelse(myData$Status == "Active",1,0)
#converts the categorical values (baseball level) to numeric values (1-7), with 7 being the highest (best)
myData<-myData%>%mutate(LevelReachedNum = case_when(
  LevelReached=="RK" ~ 1,
  LevelReached=="A-" ~ 2,
  LevelReached=="A" ~ 3,
  LevelReached=="A+" ~ 4,
  LevelReached=="AA" ~ 5,
  LevelReached=="AAA" ~ 6,
  LevelReached=="MLB" ~ 7,
  ))

#creates new df consisting of only MLB players
onlyMLB<-myData[myData$LevelReached == "MLB", ]

#creates variable with total # of observations
totalObs<-count(myData)


#Finds the average Edge Score for each of the 7 levels in pro baseball (RK, A-, A, A+, AA, AAA, MLB)
EdgeAvg <- aggregate(myData$Edge,by=list(myData$LevelReached), FUN=mean)
ConvAvg <- aggregate(myData$Convergence,by=list(myData$LevelReached), FUN=mean)
DivAvg <- aggregate(myData$Divergence,by=list(myData$LevelReached), FUN=mean)
Recog2Avg <- aggregate(myData$Recognition2,by=list(myData$LevelReached), FUN=mean)
Track2Avg <- aggregate(myData$Tracking2,by=list(myData$LevelReached), FUN=mean)

proAvgData <- aggregate(cbind(Edge,Alignment,Depth,Convergence,Divergence,Recognition2,Tracking2) ~ LevelReached, myData, mean)
#provides summary table for the Vizual Edge scores
proAvgData [1:8]

MLBAvgData <- aggregate(cbind(Edge,Alignment,Depth,Convergence,Divergence,Recognition2,Tracking2) ~ LevelReached, myData, mean)


###DATA VISUALS & SUMMARY MEASURES###

#finds number of obs for each level of baseball, out of 1233 obs
totalMLB<- sum(myData$LevelReached == "MLB")
totalAAA<- sum(myData$LevelReached == "AAA")
totalAA<- sum(myData$LevelReached == "AA")
totalA_high <- sum(myData$LevelReached == "A+")
totalA <- sum(myData$LevelReached == "A")
totalA_low <- sum(myData$LevelReached == "A-")
totalRK <- sum(myData$LevelReached == "RK")


#Figure 6
ggplot(myData, aes(LevelReached, fill = LevelReached)) +
  geom_bar() + ylab("# of Players") +xlab("Pro Baseball Level")+ stat_count(geom = "text", 
  aes(label = stat(count)),
  position=position_fill(vjust=50), colour="white")



###STATISTICAL INFERENCE###
###STATISTICAL INFERENCE###
###STATISTICAL INFERENCE###
###STATISTICAL INFERENCE###

totalbelow81<- sum(myData$Recognition2 <= 0.81)
below81_prob<- totalbelow81/totalObs 
totalbelow81
below81_prob


#reorders rows by desecending scores
EdgeAvg <- EdgeAvg[order(EdgeAvg$x,decreasing = TRUE),]

#renames the columns in EdgeAvg df to more appropriate headers
names(EdgeAvg)[names(EdgeAvg) == 'Group.1'] <- 'ProLevel'
names(EdgeAvg)[names(EdgeAvg) == 'x'] <- 'EdgeScore'
#renames the columns in ConvAvg df to more appropriate headers
names(ConvAvg)[names(ConvAvg) == 'Group.1'] <- 'ProLevel'
names(ConvAvg)[names(ConvAvg) == 'x'] <- 'ConvergenceScore'
#renames the columns in DivAvg df to more appropriate headers
names(DivAvg)[names(DivAvg) == 'Group.1'] <- 'ProLevel'
names(DivAvg)[names(DivAvg) == 'x'] <- 'DivergenceScore'
#renames the columns in Recog2Avg df to more appropriate headers
names(Recog2Avg)[names(Recog2Avg) == 'Group.1'] <- 'ProLevel'
names(Recog2Avg)[names(Recog2Avg) == 'x'] <- 'RecognitionScore'
#renames the columns in Track2Avg df to more appropriate headers
names(Track2Avg)[names(Track2Avg) == 'Group.1'] <- 'ProLevel'
names(Track2Avg)[names(Track2Avg) == 'x'] <- 'TrackingScore'


#rounds Edge Scores to 2 decimals
EdgeAvg$EdgeScore <- round(EdgeAvg$EdgeScore,2)
ConvAvg$ConvergenceScore <- round(ConvAvg$ConvergenceScore,2)
DivAvg$DivergenceScore <- round(DivAvg$DivergenceScore,2)
Recog2Avg$RecognitionScore <- round(Recog2Avg$RecognitionScore,2)
Track2Avg$TrackingScore <- round(Track2Avg$TrackingScore,2)

#uses ggplot2 library to plot bar graph of Edge Scores by pro level (Figure 7)
ggplot(EdgeAvg, aes(x = reorder(ProLevel, -EdgeScore), y = EdgeScore, fill = EdgeScore))+ geom_bar(stat = "identity") +
geom_text(aes(label = EdgeScore), vjust = 0) + coord_cartesian(ylim = c(0,100))+ ggtitle("Average Edge Score By Pro Baseball Level") + xlab("Pro Baseball Level") + ylab("Edge Score")


###START APPENDIX PLOTS###
#plots avg Convergence scores for each pro level
ggplot(ConvAvg, aes(x = reorder(ProLevel, -ConvergenceScore), y = ConvergenceScore, fill = ConvergenceScore))+ geom_bar(stat = "identity") +
  geom_text(aes(label = ConvergenceScore), vjust = 0) + coord_cartesian(ylim = c(0,60))+ ggtitle("Average Convergence Score By Pro Baseball Level") + xlab("Pro Baseball Level") + ylab("Convergence")

#plots avg Divergence scores for each pro level
ggplot(DivAvg, aes(x = reorder(ProLevel, -DivergenceScore), y = DivergenceScore, fill = DivergenceScore))+ geom_bar(stat = "identity") +
  geom_text(aes(label = DivergenceScore), vjust = 0) + coord_cartesian(ylim = c(0,30))+ ggtitle("Average Divergence Score By Pro Baseball Level") + xlab("Pro Baseball Level") + ylab("Divergence")

#plots avg Recognition scores for each pro level
ggplot(Recog2Avg, aes(x = reorder(ProLevel, RecognitionScore), y = RecognitionScore, fill = RecognitionScore))+ geom_bar(stat = "identity") +
  geom_text(aes(label = RecognitionScore), vjust = 0) + coord_cartesian(ylim = c(0,1.25))+ ggtitle("Average Recognition Score By Pro Baseball Level") + xlab("Pro Baseball Level") + ylab("Recognition Response Time (s)")

#plots avg Tracking scores for each pro level
ggplot(Track2Avg, aes(x = reorder(ProLevel, TrackingScore), y = TrackingScore, fill = TrackingScore))+ geom_bar(stat = "identity") +
  geom_text(aes(label = TrackingScore), vjust = 0) + coord_cartesian(ylim = c(0,0.60))+ ggtitle("Average Tracking Score By Pro Baseball Level") + xlab("Pro Baseball Level") + ylab("Tracking Response Time (s)")
###END APPENDIX PLOTS###


#generates a correlation plot for the numeric values in myData set (FIGURE 8)
df<- myData[,c(2:14,18:20)]
correlations <- cor(df)
corrplot(correlations,method="circle")


##GENERATES A SCATTER PLOT OF EDGE SCORES VS LEVEL REACHED
plot(myData$Edge, myData$LevelReachedNum,
xlab="Edge Score",
ylab="Level Reached")


proAvgData2 <- proAvgData[,c(1,4,5)]



#BOX PLOT#
#BOX PLOT#
#BOX PLOT#

boxplot(myData$Edge,
        main="Edge Score Boxplot",
        xlab = "Edge Score",
        horizontal = TRUE,
        col="gold",
        ylim = c(40,100))
#the boxplot shows that there ARE outliers, which are located on the LEFT with median to the RIGHT of the mean
#This suggests that Edge is NEGATIVELY skewed

#finds outliers and stores them in new variable 'outliers'
outliers<-boxplot(myData$Edge)$out
#replaces outliers with 'NA' in myData df
myData$newEdge<-ifelse(myData$Edge %in% outliers, NA, myData$Edge)

summary(myData$Edge)
summary(myData$newEdge)

Edge_mean<-mean(myData$Edge)
Edge_sd<-sd(myData$Edge)
Edge_min<-min(myData$Edge)
Edge_max<-max(myData$Edge)

MLB_mean<-mean(onlyMLB$Edge)
MLB_sd<-sd(onlyMLB$Edge)
MLB_min<-min(onlyMLB$Edge)
MLB_max<-max(onlyMLB$Edge)

#Empirical Rule

#find which values contain 68% of data
Edge_mean-Edge_sd; Edge_mean+Edge_sd
#find which values contain 95% of data
Edge_mean-2*Edge_sd; Edge_mean+2*Edge_sd
#find which values contain 99.7% of data
Edge_mean-3*Edge_sd; Edge_mean+3*Edge_sd

#68% of data between 75.45 and 87.19 (838 scores, .68 * 1233)
#95% of data between 69.57 and 93.07 (1171 scores, .95 * 1233)
#99.7% of data between 63.70 and 98.94 (1229 scores, .68 * 1233)
Edge_zMin<- (Edge_min-Edge_mean)/Edge_sd #2.1
Edge_zMax<- (Edge_max-Edge_mean)/Edge_sd #-6.8
#z-scores not useful for this data set since values are NOT symmetric/bell-shaped



#empirical probability of reaching MLB
mlb_prob<- totalMLB/totalObs # P(A) = 0.24

#counts number of observations where Edge Score is >= 83.0
totalabove83<- sum(myData$Edge >= 83.0) #525
above83_prob<- totalabove83/totalObs # P(B) = 0.426

#finds # of players with depth scores above 94%
totalabove94<- sum(myData$Depth >= .94)
above94_prob<- totalabove94/totalObs 
totalabove94
above94_prob

#finds # of players with convergence scores above 47
totalabove47<- sum(myData$Convergence >= 47)
above47_prob<- totalabove47/totalObs
above47_prob

#finds # of players with divergence scores above 27
totalabove27<- sum(myData$Divergence >= 27)
above27_prob<- totalabove27/totalObs 
totalabove27
above27_prob

#finds # of players with recognition response times (recog 2) below (quicker) than 0.81s
totalbelow81<- sum(myData$Recognition2 <= 0.81)
below81_prob<- totalbelow81/totalObs 
totalbelow81
below81_prob

#finds # of players with tracking response times (tracking2) below (quicker) than 0.49s
totalbelow49<- sum(myData$Tracking2 <= 0.49)
below49_prob<- totalbelow49/totalObs 
totalbelow49
below49_prob



#counts number of observations where Edge Score is >= 83.0 AND reached the MLB
totalMLB_83<-sum(myData$Edge >= 83.0 & myData$LevelReached == "MLB") #171
MLB_above83_prob<- totalMLB_83/totalObs #P(A ??? B) = 0.139

##calculate the probability that a player reaches the MLB, given they scored above 83.0 Edge Score
#P(A|B) = P(A ??? B) / P(B) = 0.139 / 0.426 = 0.33




## CONFIDENCE INTERVALS & HYPOTHESIS TESTING ##

lower<- Edge_mean - qt(0.975, 1232, lower.tail = TRUE)*Edge_sd/sqrt(1233)
#80.99
upper<- Edge_mean + qt(0.975, 1232, lower.tail = TRUE)*Edge_sd/sqrt(1233)
#81.65
#with 95% confidence, we can say that the average Edge Score of all players is between 80.99 - 81.65


##excluding "alternative" will produce a two-tailed test
t.test(x=myData$Edge, mu=Edge_mean, alternative="less")


#Test if the average MLB is above 83.0, n = 299 (299 players made it to MLB in this dataset)
#Ho: ?? < 83.0
#Ha: ?? ??? 83.0
#manual t-testing
t_mlb<-(83.32 - 83.0)/(MLB_sd / sqrt(299))
t.test(onlyMLB$Edge, alternative="two.sided", mu=83.32)
#p-value = 0.9963, thus we do NOT reject the NUll


###Linear Regression ###

##turn off scientific notation
options(scipen=999)
# 
# ## X linear regression models to predict exact pro level reached (RK-MLB, 1-7)
# linr1<-lm(LevelReachedNum~Depth+Convergence+Divergence+Recognition1+Recognition2+Tracking1+Tracking2+DraftAge+CollegePlayer+DraftPick, data=myData)
# summary(linr1)
# linr2<-lm(LevelReachedNum~Depth+Convergence+Divergence+Recognition1+Recognition2+Tracking1+Tracking2+CollegePlayer+DraftPick, data=myData)
# summary(linr2)
# linr3<-lm(LevelReachedNum~Depth+Convergence+Divergence+Recognition1+Recognition2+Tracking1+Tracking2+DraftAge+DraftPick, data=myData)
# summary(linr3)
# linr4<-lm(LevelReachedNum~Depth+Convergence+Divergence+Recognition2+Tracking2+CollegePlayer+DraftPick, data=myData)
# summary(linr4)
# linr5<-lm(LevelReachedNum~Edge+DraftAge+DraftPick+CollegePlayer, data=myData)
# summary(linr5)
# linr6<-lm(LevelReachedNum~Edge+DraftPick+CollegePlayer, data=myData)
# summary(linr6)

# #compare best model against itself with interaction variable(s)
# linr2<-lm(LevelReachedNum~Depth+Convergence+Divergence+Recognition1+Recognition2+Tracking1+Tracking2+CollegePlayer+DraftPick, data=myData)
# summary(linr2)
# ##Interaction variable used
# linr2<-lm(LevelReachedNum~Depth+Convergence+Divergence+Recognition1+Recognition2+Tracking1+Tracking2+(Recognition1*Tracking1)+(Recognition2*Tracking2)+CollegePlayer+DraftPick, data=myData)
# summary(linr2)
# R-squared increases by 0.001 as a result of the interaction variable



###LOGISTIC REGRESSION###

logr1 <- glm(ReachedMLB ~ Convergence+Divergence+Recognition2, family = binomial(link = logit), data = myData)
summary(logr1)
##predict computes the probabilities for given sample values
Pred <- predict(logr1, type = "response")
##'round' allows us to derive the binary predicted values
Binary <- round(Pred)
##here we use 'mean' to compute the proportion of correctly classified cases (*100 for percentage)
100*mean(myData$ReachedMLB == Binary)

logr1 <- glm(ReachedMLB ~ Edge+Depth+Convergence+Divergence+DraftAge+DraftPick, family = binomial(link = logit), data = myData)
summary(logr1)
##predict computes the probabilities for given sample values
Pred <- predict(logr1, type = "response")
##'round' allows us to derive the binary predicted values
Binary <- round(Pred)
##here we use 'mean' to compute the proportion of correctly classified cases (*100 for percentage)
100*mean(myData$ReachedMLB == Binary)


logr2 <- glm(ReachedMLB ~ Edge+Convergence+Divergence+DraftAge+DraftPick, family = binomial(link = logit), data = myData)
summary(logr2)
##predict computes the probabilities for given sample values
Pred <- predict(logr2, type = "response")
##'round' allows us to derive the binary predicted values
Binary <- round(Pred)
##here we use 'mean' to compute the proportion of correctly classified cases (*100 for percentage)
100*mean(myData$ReachedMLB == Binary)


logr3 <- glm(ReachedMLB ~ Edge+DraftAge+DraftPick, family = binomial(link = logit), data = myData)
summary(logr3)
##predict computes the probabilities for given sample values
Pred <- predict(logr3, type = "response")
##'round' allows us to derive the binary predicted values
Binary <- round(Pred)
##here we use 'mean' to compute the proportion of correctly classified cases (*100 for percentage)
100*mean(myData$ReachedMLB == Binary)

plot(myData$Convergence, myData$LevelReachedNum)


#Edge+CollegePlayer+DraftPick = 80.12976
#Convergence+CollegePlayer+DraftPick = 80.29197
#Depth+Convergence+CollegePlayer+DraftPick = 80.45418
#Depth+DraftAge+DraftPick = 80.37307
#Edge+Depth+Convergence+Divergence+DraftAge+DraftPick = 80.69749 ***********************
#Edge+Convergence+Divergence+DraftAge+DraftPick = 80.69749 ***********************

#turns the numeric values of 1 & 0 for ReachedMLB into factor type so R can understand it
myData$ReachedMLB<-as.factor(myData$ReachedMLB)

#myControl<-trainControl(method = "cv", number = 4)

myControl<-trainControl(method = "repeatedcv", number = 4, repeats = 5)

##
Model1 <- train(ReachedMLB ~ Edge+Depth+Convergence+Divergence+DraftAge+DraftPick, 
                data = myData,
                trControl = myControl,
                method = "glm",
                family = binomial (link = logit),
                metric = "Accuracy")
Model1
#~79.7%

Model2 <- train(ReachedMLB ~ Edge+Convergence+Divergence+DraftAge+DraftPick, 
                data = myData,
                trControl = myControl,
                method = "glm",
                family = binomial (link = logit),
                metric = "Accuracy")
Model2
#~79.8%

Model3 <- train(ReachedMLB ~ Depth+Convergence+CollegePlayer+DraftPick, 
                data = myData,
                trControl = myControl,
                method = "glm",
                family = binomial (link = logit),
                metric = "Accuracy")
Model3
#~79.7%

