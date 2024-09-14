# QUESTION 1
# A
baseballlarge <- read.csv("baseballlarge.csv")
str(baseballlarge)
# (i)
print(length(unique(baseballlarge$Team)))
# (ii)
table(baseballlarge$Year)
# (iii)
baseballlarge <- subset(baseballlarge, baseballlarge$Playoffs==1)
# (iv)
length(unique(baseballlarge$Team))
table(baseballlarge$Team)
table(baseballlarge$Year)

# B
baseballlarge$NumCompetitors <- table(baseballlarge$Year)[as.character(baseballlarge$Year)]
df <- subset(baseballlarge, baseballlarge$NumCompetitors==8)
str(df)

# C
baseballlarge$WorldSeries <- as.integer(baseballlarge$RankPlayoffs == 1)

# QUESTION 2
# A
Parole <- read.csv("Parole.csv")
str(Parole)
print(length(Parole$Male))

# B
str(subset(Parole, Parole$Violator == 1))
table(Parole$Violator)

# C
table(Parole$State)

# D
set.seed(144)
library(caTools)
split <- sample.split(Parole$Violator, SplitRatio = 0.7)
train <- subset(Parole, split == TRUE)
test <- subset(Parole, split == FALSE)

# E
model_1 <- glm(Violator~., data=train, family=binomial)
summary(model_1)

# F
odds <- exp(coef(model_1)["MultipleOffenses"])
# statement 4 is the correct answer

# G
logodds <- model_1$coef[1] + model_1$coef[2]*1 + model_1$coef[3]*1 + model_1$coef[4]*50 + model_1$coef[6] + model_1$coef[8]*3 + model_1$coef[9]*12 + model_1$coef[12]*1
logodds
odds <- exp(logodds)
odds
prob <- exp(logodds)/ (1+exp(logodds))
prob

# H
pred <- predict(model_1, newdata = test, type = "response")
pred

# I
pred_table <- table((pred > 0.5), test$Violator)
pred_table
TP <- pred_table[2,2]
TN <- pred_table[1,1]
FN <- pred_table[1,2]
FP <- pred_table[2,1]
sensitivity <- TP / (TP+FN)
sensitivity
specificity <- TN / (FP+TN)
specificity

accuracy <- sum(diag(pred_table))/sum(pred_table)
accuracy

# J
pred_table <- table((pred > 0), test$Violator)
pred_table
accuracy <- sum(diag(pred_table))/sum(pred_table)
accuracy

# K
# 2nd statement

# L
pred_table <- table((pred > 0.5), test$Violator)
pred_table
FP_rate <- FP/sum(pred_table)
FP_rate
FN_rate <- FN/sum(pred_table)
FN_rate
# 4th statement. Current model already outperforms baseline model since it is make predictions with accuracy greater than 50% 
# However, we are still able to improve the current model as there are many predictor variables which are insignificant, weakening the model. 

# M
library(ROCR)
?prediction
pred <- predict(model_1, newdata = test, type = "response")
ROCR_pred <- prediction(pred, test$Violator)
ROCR_perf <- performance(ROCR_pred,x.measure="fpr",measure="tpr")
plot(ROCR_perf)
aucperf <- performance(ROCR_pred,measure="auc")
str(aucperf)
aucperf@y.values

# N
# 1st statement is correct. 

# O
# 4th statement is correct



































