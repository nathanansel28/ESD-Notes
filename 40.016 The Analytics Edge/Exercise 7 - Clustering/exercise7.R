# ============
#  QUESTION 1
# ============
# (a)
stocks <- read.csv("StocksCluster.csv")
str(stocks)

# (b)
table(stocks$PositiveDec)
6324 / (6324 + 5256)
# 0.546114

# (c)
corr_object <- cor(subset(stocks, select=-c(PositiveDec)))
maximum <- 0
for (i in 1:length(corr_object)){
  if (corr_object[i] != 1 & corr_object[i] > maximum){
    maximum <- corr_object[i]
  }
}
which(corr_object == maximum)
# October and November

# (d)
mean_table <- apply(subset(stocks, select=-c(PositiveDec)), 2, mean)
which.max(mean_table)
which.min(mean_table)
# max: april
# min: september

# (e) 
library(caTools)
set.seed(144)
spl <- sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain <- subset(stocks, spl == TRUE)
stocksTest <- subset(stocks, spl == FALSE)

StocksModel <- glm(as.factor(PositiveDec) ~ ., data=stocksTrain, family=binomial)
summary(StocksModel)

StocksModel.pred <- predict(StocksModel, newdata=stocksTrain, type="response")
StocksModel.confusionmatrix <- table(StocksModel.pred >= 0.5, stocksTrain$PositiveDec)
StocksModel.confusionmatrix
StocksModel.accuracy <- sum(diag(StocksModel.confusionmatrix)) / sum(StocksModel.confusionmatrix)
StocksModel.accuracy
# accuracy: 0.5711818

# (f) 
StocksModel.predtest <- predict(StocksModel, newdata=stocksTest, type="response")
StocksModel.confusionmatrix <- table(StocksModel.predtest >= 0.5, stocksTest$PositiveDec)
StocksModel.confusionmatrix
StocksModel.accuracy <- sum(diag(StocksModel.confusionmatrix)) / sum(StocksModel.confusionmatrix)
StocksModel.accuracy
# accuracy: 0.5670697

# (g)
baseline_pred_test <- rep(ceiling(mean(stocksTrain$PositiveDec)), nrow(stocksTest))
confusion_matrix <- table(baseline_pred_test >= 0.5, stocksTest$PositiveDec)
confusion_matrix
baseline_accuracy <- confusion_matrix[2] / sum(confusion_matrix)
baseline_accuracy
# accuracy: 0.5460564

# (h)
limitedTrain <- stocksTrain
limitedTrain$PositiveDec <- NULL
limitedTest <- stocksTest
limitedTest$PositiveDec <- NULL
# Why do we need to remove the dependent variable in the clustering phase of the 
# cluster-then-predict methodology?
# Answer: (iii) Needing to know the dependent variable value to assign an observation to 
#         a cluster defeats the purpose of the methodology
# Reason: In real life, we don't observe the dependent variable PositiveDec until it is in the past.

# (i)
library(caret)
preproc <- preProcess(limitedTrain)
normTrain <- predict(preproc, limitedTrain)
normTest <- predict(preproc, limitedTest)
mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)
# 2.100586e-17
# -0.0004185886
# both are very close to 0

# (j) 
# Why is the mean ReturnJan variable much closer to 0 in normTrain than in normTest?
# Answer: (iii) since the distribution in training and test set are not identical 
#               and normalization is done on the training set.

# (k)
set.seed(144)
km <- kmeans(normTrain,centers=3)
summary(km)
table(km$cluster)
# It is clear that cluster 2 has the largest number of observations.

# (l)
library(flexclust)
km.kcca <- as.kcca(km, normTrain)
clusterTrain <- predict(km.kcca)
clusterTest <- predict(km.kcca, newdata=normTest)
table(clusterTest)
# 2029 observations in cluster 2

# (m)
stocksTrain1 <- subset(stocksTrain, clusterTrain==1)
stocksTrain2 <- subset(stocksTrain, clusterTrain==2)
stocksTrain3 <- subset(stocksTrain, clusterTrain==3)
mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)
# StocksTrain1 has the highest return

# (n)
StocksModel1 <- glm(as.factor(PositiveDec)~., data=stocksTrain1, family=binomial)
StocksModel2 <- glm(as.factor(PositiveDec)~., data=stocksTrain2, family=binomial)
StocksModel3 <- glm(as.factor(PositiveDec)~., data=stocksTrain3, family=binomial)
summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)
# At least 1 positive sign for the coefficient in any of the models:
#   Intercept, Jan, Feb, Apr, May, June, July, Sep
#   Mar, Aug, Oct
#   

# At least 1 negative sign for the coefficient in any of the models:
#   Mar, Aug, Oct, Nov
#   Intercept, Feb
# Therefore, Feb, Mar, July and Sep have positive and negative coefficients in the models

# (o)
stocksTest1 <- subset(stocksTest, clusterTest==1)
stocksTest2 <- subset(stocksTest, clusterTest==2)
stocksTest3 <- subset(stocksTest, clusterTest==3)
PredictTest1 <- predict(StocksModel1, newdata=stocksTest1, type="response")
PredictTest2 <- predict(StocksModel2, newdata=stocksTest2, type="response")
PredictTest3 <- predict(StocksModel3, newdata=stocksTest3, type="response")
StocksModel1.cm <- table(PredictTest1 >= 0.5, stocksTest1$PositiveDec)
StocksModel2.cm <- table(PredictTest2 >= 0.5, stocksTest2$PositiveDec)
StocksModel3.cm <- table(PredictTest3 >= 0.5, stocksTest3$PositiveDec)
StocksModel1.cm
StocksModel2.cm
StocksModel3.cm
StocksModel1.testaccuracy <- sum(diag(StocksModel1.cm)) / sum(StocksModel1.cm)
StocksModel2.testaccuracy <- sum(diag(StocksModel2.cm)) / sum(StocksModel2.cm)
StocksModel3.testaccuracy <- sum(diag(StocksModel3.cm)) / sum(StocksModel3.cm)
StocksModel1.testaccuracy
StocksModel2.testaccuracy
StocksModel3.testaccuracy
# 0.6446125
# 0.5367176
# 0.625323

# (p)
AllPredictions <- c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes <- c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
overall_cm <- table(AllPredictions >= 0.5, AllOutcomes)
overall_cm
overall_accuracy <- sum(diag(overall_cm)) / sum(overall_cm)
overall_accuracy
# 0.5794473


# ============
#  QUESTION 2
# ============

# (a) 
citi <- read.csv("citibike.csv")
str(citi)
unique(citi$startstation)
unique(citi$endstation)
length(unique(citi$startstation))
length(unique(citi$endstation))
sort(unique(citi$startstation)) == sort(unique(citi$endstation))
# 329

# (b)
maximum <- 0 
day <- NULL
for (i in 1:length(unique(citi$day))){
  day_ <- unique(citi$day)[i]
  df <- subset(citi, day == day_)
  df
  mean_ <- mean(df$tripduration)
  cat(mean_)
  if (mean_ > maximum){
    maximum <- mean_ 
    day <- day_
  } 
}
day
maximum
# "Sat"
# 894.2661

tapply(citi$tripduration,citi$day,mean)

# (c)
tab <- table(citi$starttime)
which.max(tab)
which.min(tab)
# start hour when max: 18
# start hour when min: 4

# (d)
tab <- table(citi$gender)
tab[2] / (tab[1]+tab[2])
# 0.2349904 

# (e)
citi$Mon <- ifelse(citi$day == "Mon", 1, 0)
citi$Tue <- ifelse(citi$day == "Tue", 1, 0)
citi$Wed <- ifelse(citi$day == "Wed", 1, 0)
citi$Thu <- ifelse(citi$day == "Thu", 1, 0)
citi$Fri <- ifelse(citi$day == "Fri", 1, 0)
citi$Sat <- ifelse(citi$day == "Sat", 1, 0)
citi$Sun <- ifelse(citi$day == "Sun", 1, 0)

# (f)
mean(citi$tripduration)
mean(citi$gender)
mean(citi$age)
mean(citi$starttime)
mean(citi$Mon)
# Since trip duration on average has the highest value, 
# it is likely to dominate the distance calculations

# (g)
citi$tripduration <- scale(citi$tripduration)
citi$gender <- scale(citi$gender)
citi$age <- scale(citi$age)
citi$starttime <- scale(citi$starttime)
citi$Mon <- scale(citi$Mon)
citi$Tue <- scale(citi$Tue)
citi$Wed <- scale(citi$Wed)
citi$Thu <- scale(citi$Thu)
citi$Fri <- scale(citi$Fri)
citi$Sat <- scale(citi$Sat)
citi$Sun <- scale(citi$Sun)
max(citi$tripduration)
# 402.9514

# (h) We will not use hierarchical clustering for this dataset. 
# Why do you think hierarchical clustering might have a problem with this dataset?
# Answer: The dataset is too large in size, and since hierarchical clustering is a bottom-up approach, it is not the most efficient method 

# (i)
set.seed(100)
clustering <- kmeans(subset(citi, select=c(tripduration, gender, age, starttime, Mon, Tue, Wed,
                                           Thu, Fri, Sat, Sun)), centers=10)
clustering
tab <- table(clustering$cluster)
which.max(tab)
which.min(tab)
# largest cluster: 3
# smallest cluster: 4 

# (j)
# Judging from the cluster means: 
# Clusters 6 and 7 has the highest age mean 
# Clusters 6 and 7 has the same sat mean
# So clusters 6 and 7 are likely to represent trips taken primarily by older users on Saturdays


# (k)
# Judging from the cluster means:
# Clusters 1, 8, and 10 has the closest gender to female on average
# Cluster 1 has a high Tuesday value and a lower Wednesday value
# Cluster 8 has a has a low Tuesday value and a relatively higher Wednesday value
# Cluster 10 has both relatively low Tuesday and Wednesday value, and both are equal
# So cluster 10 is likely to represent trips made by female either on Tue or Wed

# (l)
# Different results from the first k-means clustering 
# because the clustering result is dependent on the initial centroid points 

# (m)
# The same results, since the seed ensures that everything is exactly reproducible 

# (n)
citi <- read.csv("CitiBike.csv")
citi$weekday <- ifelse((citi$day != "Sat" & citi$day != "Sun"), 1, 0)
citi$weekday <- scale(citi$weekday)
citi$tripduration <- scale(citi$tripduration)
citi$gender <- scale(citi$gender)
citi$age <- scale(citi$age)
citi$starttime <- scale(citi$starttime)

set.seed(100)
clustering_n <- kmeans(subset(citi, select=c(tripduration, gender, age, starttime, weekday)), centers=10)
clustering_n
table(clustering_n$cluster)
# longer trips taken by older female users on weekdays: 
# Clusters 1, 8, and 9 are dominated by female users
# Cluster 1 has a relatively long trip duration
# Cluster 8 has a higher trip duration than cluster 1
# Cluster 9 has the lowest trip duration than cluster 1 and 8
# Cluster 1 and 8 have mild weekday values, but cluster 1 has a higher weekday value
# Either cluster 1 or 8 represent this group of users

# (o)
# short trips taken by younger male users early on weekdays
# Male: clusters 2, 3, 5, 6, 7, 10
# Weekdays: clusters 1, 3, 5, 6, 7, 8, 9
# Early: clusters 3, 7, 9, 10
# Cluster 7 is likely to represent this group of users


# ============
#  QUESTION 3
# ============
# ------------------
# Data Extraction
# ------------------
ratings <- read.csv("ratings.csv")
str(ratings)

Data <- matrix(nrow=length(unique(ratings$userId)), ncol=length(unique(ratings$movieId)))
rownames(Data) <- unique(ratings$userId)
colnames(Data) <- unique(ratings$movieId)
# the rows indicate the user
# the columns indicate the movie 
Data

for(i in 1:nrow(ratings)){
  Data[as.character(ratings$userId[i]),as.character(ratings$movieId[i])] <- ratings$rating[i]
}
dim(Data)
Datanorm <- Data - rowMeans(Data,na.rm=TRUE)

# ------------------
# Train Test Split
# ------------------
set.seed(1)       
spl1 <- sample(1:nrow(Data), 0.98*nrow(Data)) # spl1 has 98% of the rows
spl1c <- setdiff(1:nrow(Data),spl1)           # spl1c has the remaining ones
set.seed(2)
spl2 <- sample(1:ncol(Data), 0.8*ncol(Data))  # spl2 has 80% of the columns
spl2c <- setdiff(1:ncol(Data),spl2)           # spl2c has the rest

Base1    <- matrix(nrow=length(spl1c), ncol=length(spl2c))  # matrix to store predictions from baseline model 1
Base2    <- matrix(nrow=length(spl1c), ncol=length(spl2c))  # matrix to store predictions from baseline model 2
UserPred <- matrix(nrow=length(spl1c), ncol=length(spl2c))  # matrix to store predictions from user model

# --------------------------
# Baseline Model 1 & 2
# --------------------------
for(i in 1:length(spl1c)){
  Base1[i,] <- colMeans(Data[spl1,spl2c], na.rm=TRUE)
}
for(j in 1:length(spl2c)){
  Base2[,j] <- rowMeans(Data[spl1c,spl2], na.rm=TRUE)
}

# --------------------------
# User Based Model
# --------------------------
Cor   <- matrix(nrow=length(spl1),ncol=1) # keeps track of the correlation between users
Order <- matrix(nrow=length(spl1c), ncol=length(spl1)) # sort users in term of decreasing correlations
for(i in 1:length(spl1c)){
  # i       : user in the 2% of split
  for(j in 1:length(spl1)){
    # j     : user in the 98% of split
    # spl1c : split of 2% of users
    # spl1  : split of 98% of users
    # spl2  : split of 80% of movies
    Cor[j] <- cor(Data[spl1c[i],spl2], Data[spl1[j],spl2], use = "pairwise.complete.obs")
    # Cor[j] computes the correlation between users from split spl1c and spl1 who rate movie spl2
  }
  V <- order(Cor, decreasing=TRUE, na.last=NA)
  Order[i,] <- c(V, rep(NA, times=length(spl1)-length(V)))
}
Cor
Order

# --------------------------
# Predictions
# --------------------------
for(i in 1:length(spl1c)){
  UserPred[i,] <- colMeans(Data[spl1[Order[i,1:250]],spl2c], na.rm=TRUE)
  # here, we arelooking at the 250 nearest neighbours
}
RMSEBase1    <- sqrt(mean((Data[spl1c,spl2c] - Base1)^2, na.rm=TRUE)) 
RMSEBase2    <- sqrt(mean((Data[spl1c,spl2c] - Base2)^2, na.rm= TRUE)) 
RMSEUserPred <- sqrt(mean((Data[spl1c,spl2c] - UserPred)^2,na.rm=TRUE)) 

RMSE <- rep(NA, times=490)
for(k in 10:499)
{for(i in 1:length(spl1c))
{UserPred[i,] <- colMeans(Data[spl1[Order[i,1:k]],spl2c], na.rm=TRUE)}
  RMSE[k-10] <- sqrt(mean((Data[spl1c,spl2c] - UserPred)^2,na.rm=TRUE))}
plot(10:499,RMSE,pch=20,lwd=0.5)

# --------------------------
# Weighted User Based Model
# --------------------------
w.avg <- function(x,y){
  # If y'all want to use weighted.mean instead, take note to include
  # the separate argument "na.rm=TRUE" in the apply() function call
  # z <- weighted.mean(x,y,na.rm=TRUE)
  z <- sum(x*y, na.rm=TRUE)/sum(y[which(!is.na(x))])
  if (!is.na(z)){
    if(z>5){z <- 5}
    if(z<0){z <- 0}
  }
  return(z)
}

# Compute the correlation and the order
Cor <- matrix(nrow=length(spl1c),ncol=length(spl1))
Order <- matrix(nrow=length(spl1c), ncol=length(spl1)) 
suppressWarnings(
  for (i in 1:length(spl1c)){
    for (j in 1:length(spl1)){
      Cor[i,j] <- cor(Data[spl1c[i], spl2], Data[spl1[j], spl2], use="pairwise.complete.obs")
    }
    V <- order(Cor[i,], decreasing=TRUE, na.last=NA)
    Order[i,] <- c(V, rep(NA, times=length(spl1)- length(V)))
  }
)

# Iterate through each user in the test set, and make the predictions
numk <- c(10,50,100,150,200,250,300)
RMSE <- rep(NA, times=length(numk))
for(ind in 1:length(numk)){
  k <- numk[ind]
  UserPred <- matrix(nrow=length(spl1c), ncol=length(spl2c))
  for(i in 1:length(spl1c)){
    # For every user i in the test set
    w <- Cor[i,Order[i,1:k]]              # extract the correlation of the nearest k neighbours
    D <- Data[spl1[Order[i,1:k]],spl2c]   # extract the real rating of the nearest k neighbours 
    UserPred[i,] <- apply(D,2,w.avg,y=w)  # make the weighted average predictions
  }
  RMSE[ind] <- sqrt(mean((Data[spl1c,spl2c] - UserPred)^2,na.rm=TRUE))
}

plot(numk,RMSE)


























