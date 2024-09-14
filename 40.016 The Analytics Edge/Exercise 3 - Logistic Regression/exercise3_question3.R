# QUESTION 3
rm(list = ls())

# A
germancredit <- read.csv("germancredit.csv")
set.seed(2019)
library(caTools)
spl <- sample.split(germancredit$resp, 0.75)
training <- subset(germancredit, spl == TRUE)
test <- subset(germancredit, spl == FALSE)
# option 2: we use the sample.split function to balance the independent variables between train and test set. 

# B
model_intercept <- glm(resp~1, data=training, family=binomial) 
summary(model_intercept)
# P(resp=1) = exp(0.84730) / (1 + exp(0.84730))

# C
# Provide a precise mathematical relationship between the estimated coefficient and
# the fraction of respondents with a good credit rating in the training set.
table(training$resp)
table(training$resp)[2]/sum(table(training$resp))
525 / (525+225)
# 0.7 is = P(resp=1) = exp(0.84730) / (1 + exp(0.84730))

# D
model_1 <- glm(resp~., data=training, family=binomial)
summary(model_1)
p_val <- summary(model_1)$coefficients[,4]
names(subset(p_val, p_val <= 0.1))

# E
logLik(model_1)

# F
pred <- predict(model_1, newdata = test, type = "response")
pred_table <- table((pred > 0.5), test$resp)
pred_table

# G
TP <- pred_table[2,2]
TP
TN <- pred_table[1,1]
TN
FN <- pred_table[1,2]
FN
FP <- pred_table[2,1]
FP

sensitivity <- TP / (TP+FN)
sensitivity
specificity <- TN / (FP+TN)
specificity
accuracy <- sum(diag(pred_table))/sum(pred_table)
accuracy

# H
model_2 <- glm(resp ~ -1 + chkacct+hist+newcar+amt+sav+emp+instrate+malesingle+guar+other+for., data = training, family = binomial)
summary(model_2)
model_2$aic

# I
print("Is model 2 preferable?")
model_1$aic > model_2$aic

# J
pred <- predict(model_2, newdata = test, type = "response")
pred_table <- table((pred > 0.5), test$resp)
pred_table

# K
# which model is better when it comes to fraction of people
# predicted as good credit risk but are actually bad credit risk in the test set
# i.e. FALSE POSITIVE
pred_1 <- predict(model_1, newdata = test, type = "response")
pred_table_1 <- table((pred_1 > 0.5), test$resp)
FP_1 <- pred_table_1[2,1]
TN_1 <- pred_table_1[1,1]
FPR_1 <- FP_1 / (FP_1+TN_1)
FPR_1

pred_2 <- predict(model_2, newdata = test, type = "response")
pred_table_2 <- table((pred_2 > 0.5), test$resp)
FP_2 <- pred_table_2[2,1]
TN_2 <- pred_table_2[1,1]
FPR_2 <- FP_2 / (FP_2+TN_2)
FPR_2

print("Model 1 is better than model 2 in this regard")
FPR_1 < FPR_2 

# L
# based on fraction of people who are predicted as bad credit risk but are actually good
# credit risk in the test set, which model is preferable?
# i.e. FALSE NEGATIVE RATE
pred_1 <- predict(model_1, newdata = test, type = "response")
pred_table_1 <- table((pred_1 > 0.5), test$resp)
FN_1 <- pred_table_1[1,2]
TP_1 <- pred_table_1[2,2]
FNR_1 <- FN_1 / (TP_1 + FN_1)
FNR_1

pred_2 <- predict(model_2, newdata = test, type = "response")
pred_table_2 <- table((pred_2 > 0.5), test$resp)
FN_2 <- pred_table_2[1,2]
TP_2 <- pred_table_2[2,2]
FNR_2 <- FN_2 / (TP_2 + FN_2)
FNR_2

print("Model 2 is better than model 1 in this regard")
FNR_1 > FNR_2 

# M: calculating AUC
library(ROCR)
ROCR_pred <- prediction(pred_1, test$resp) 
ROCR_perf_1 <- performance(ROCR_pred,measure = "auc")@y.values 
ROCR_perf <- performance(ROCR_pred,x.measure="fpr",measure="tpr") 
plot(ROCR_perf)

ROCR_pred <- prediction(pred_2, test$resp) 
ROCR_perf_2 <- performance(ROCR_pred,measure = "auc")@y.values 
ROCR_perf <- performance(ROCR_pred,x.measure="fpr",measure="tpr") 
plot(ROCR_perf)

ROCR_perf_1 
ROCR_perf_2
print("model 1 is better")

# N 
pred_1 <- predict(model_1, newdata = test, type = "response")
pred_table_1 <- table((pred_1 >= 0.5), test$resp)
pred_table_1

profit_1 <- pred_table_1[2,1] * (-300) + pred_table_1[2,2] * 100
profit_1

# O
pred_1 <- predict(model_1, newdata = test, type = "response")
pred_1_sorted <- sort(pred_1, decreasing = TRUE)
head(pred_1)
pred_1_sorted
germancredit[names(pred_1_sorted[length(pred_1_sorted)]),]$dur

# P
pred_table_1_sorted <- table((pred_1_sorted> 0.5), test$resp)
pred_table_1
pred_table_1_sorted


















