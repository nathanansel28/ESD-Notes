# ============
#  QUESTION 1
# ============
census <- read.csv("census.csv")
head(census)
str(census)
census$over50k <- as.integer(census$over50k == " >50K")

library(caTools)
set.seed(2000)
split <- sample.split(census$over50k, SplitRatio=0.60)
train <- subset(census,split==TRUE)
test <- subset(census,split==FALSE)
mean(census$over50k)

# (a)
model_1 <- glm(over50k ~ ., data=train, family="binomial")
summary(model_1)
logLik(model_1)
# 'log Lik.' -6052.197 (df=97)

# (b)
pred <- predict(model_1, newdata = test, type = "response")
pred_table <- table((pred >= 0.5), test$over50k)
pred_table
TP <- pred_table[2,2]
TN <- pred_table[1,1]
FN <- pred_table[1,2]
FP <- pred_table[2,1]
sensitivity <- TP / (TP+FN)
specificity <- TN / (FP+TN)
FPR <- FP / (TN + FP)
FNR <- FN / (TP + FN)
accuracy <- sum(diag(pred_table))/sum(pred_table)
accuracy
# 0.8552107

# (c)
# The baseline model simply predicts the majority in the training set which is 0 
# (less than or equal to 50 K) in the training set.
tab <- table(train$over50k)
baseline_accuracy <- tab[1]/(tab[1]+tab[2])
baseline_accuracy
# 0.7593683 

# (d)
library(ROCR)
ROCRpred <- prediction(pred, test$over50k)
performance(ROCRpred, "auc")@y.values
# 0.9061598

# (e) 
library(rpart)
library(rpart.plot)
tree1 <- rpart(as.factor(over50k) ~., data = train)
prp(tree1)

# (f) 
# number of split nodes: 3
# number of leaf nodes: 5

# (g)
# relationship 

# (h)
# capitalg
# education

# (i)
pred.tree <- predict(tree1, newdata = test, type = "class")
table2 <- table(test$over50k, pred.tree)
sum(diag(table2))/sum(table2)
# 0.8473927

# (j)
# logistic regression
perf1 <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")
plot(perf1)

# decision trees
TREEpred <- predict(tree1, newdata = test, type = "prob")
ROCRpred2 <- prediction(TREEpred[,2], test$over50k)
perf2 <- performance(ROCRpred2, measure = "tpr", x.measure = "fpr")
plot(perf2)

# (k)
performance(ROCRpred2, measure = "auc")@y.values
# 0.8470256

# (l)
set.seed(1)
trainSmall <- train[sample(nrow(train), 2000), ]
set.seed(1)
library(randomForest)
forest <- randomForest(as.factor(over50k) ~ ., data=trainSmall)
forest
predictforest <- predict(forest, newdata=test)
predictforest_tab <- table(test$over50k, predictforest)
accuracy <- sum(diag(predictforest_tab)) / sum(predictforest_tab)
accuracy
# 0.8410601

# (m)
vu <- varUsed(forest, count=TRUE)
vusorted <- sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(forest$forest$xlevels[vusorted$ix]))
# most important variables: age, occupation, hoursperweek, education

# (n)
varImpPlot(forest)
# most important variables: age, capitalgain, education

# (o)
library(rpart)
library(rpart.plot)
cart1 <- rpart(over50k~., data=train, method="class", cp=0.0001)
prp(cart1) 

printcp(cart1)
prp(cart1,type=4,extra=4)
prp(cart1,type=4,extra=9) # "normalized" probabilities 
# extra=9 -> probabilities times fraction of observations at the node (the sum across all leaves is 1)
library(rattle)
library(RColorBrewer)
fancyRpartPlot(cart1)
printcp(cart1)
plotcp(cart1)

# Find lowest CVE and prune the tree
opt <- which.min(cart1$cptable[,"xerror"]) ##  get index of CP with lowest xerror
selected_cp <- cart1$cptable[opt, "CP"]            ##  get the corresponding value
selected_cp
cart2 <- prune(cart1, selected_cp)
prp(cart2)

# (p)
pred_cart2 <-  predict(cart2, newdata=test, type="class")
cm_cart2 <- table(pred_cart2, test$over50k)
cm_cart2

accuracy <- sum(diag(cm_cart2)) / sum(cm_cart2)
accuracy


# ============
#  QUESTION 2
# ============
rm(list=ls())
set.seed(1)
boston <- read.csv("boston.csv")
split <- sample(1:nrow(boston), 0.5*nrow(boston))
train <- boston[split, ] 
test <- boston[-split, ]

# (a) 
r_tree1 <- rpart(medv~., data=train)
prp(r_tree1,type=4, extra=1)
# variables used: rm, lstat, crim, age

# (b)
pred1 <- predict(r_tree1, newdata=test)
mse1 <- sum((pred1 - test$medv)**2) / nrow(test)
mse1 
# mse = 35.28688
plot(pred1, test$medv)
abline(1:50,1:50)

# (c) 
printcp(r_tree1)
plotcp(r_tree1)
opt <- which.min(r_tree1$cptable[,"xerror"]) ##  get index of CP with lowest xerror
selected_cp <- r_tree1$cptable[opt, "CP"]            ##  get the corresponding value
selected_cp
# yes, prune the tree 
r_tree1.1 <- prune(r_tree1, selected_cp)
prp(r_tree1.1)

# (d) 
pred1.1 <- predict(r_tree1.1, newdata=test)
mse1.1 <- sum((pred1.1 - test$medv)**2) / nrow(test)
mse1.1
# mse = 35.16439
# results are not much better

# (e) 
library(randomForest)
library(rpart)
library(rpart.plot)

set.seed(1)
forest_1 <- randomForest(medv~., data=train)
forest_1
predforest_1 <- predict(forest_1, newdata=test)
mse_predforest_1 <- sum((predforest_1 - test$medv)**2) / nrow(test)
mse_predforest_1
print(forest_1)
# the random forest model produced a significantly lower MSE
# 4 variables tried in each split

# (f) 
# importance(forest_1)
varImpPlot(forest_1)

# (g)
# The larger the mtry, the larger the number of random predictor variables selected in each tree split in the RF model
# This means that there is a larger chance that the significant predictor variables are selected in the beginning of the tree
# Which further implies that the model will have a higher variance due to having more correlated trees

# ============
#  QUESTION 3
# ============
rm(list=ls())
supreme <- read.csv("supremeexercise.csv")
# 1 = conservative
# 0 = liberal

# (a)
supreme$lctdir <- as.integer(supreme$lctdir == "conser")
supreme$lctdir
supreme$reversed <- as.integer(supreme$lctdir != supreme$result)
supreme$reversed
table(supreme$reversed)
mean(supreme$reversed)
# 61.53846% of the lower court rulings were reversed

# (b)
# unCons = unanimous conservative decision
supreme$unCons <- as.integer(supreme$rehndir == 1 & 
                             supreme$stevdir == 1 & 
                             supreme$ocondir == 1 &
                             supreme$scaldir == 1 &
                             supreme$kendir == 1 &
                             supreme$soutdir == 1 &
                             supreme$thomdir == 1 &
                             supreme$gindir == 1 &
                             supreme$brydir == 1)
total_unCons <- sum(supreme$unCons)
total_unCons
# 143

# (c)
supreme$unLib <- as.integer(supreme$rehndir == 0 & 
                               supreme$stevdir == 0 & 
                               supreme$ocondir == 0 &
                               supreme$scaldir == 0 &
                               supreme$kendir == 0 &
                               supreme$soutdir == 0 &
                               supreme$thomdir == 0 &
                               supreme$gindir == 0 &
                               supreme$brydir == 0)
total_unLib <- sum(supreme$unLib)
total_unLib
# 124


# (d)
cart_cons <- rpart(as.factor(unCons) ~ petit+respon+circuit+unconst+lctdir+issue, data=supreme)
prp(cart_cons)
print(cart_cons)
# 6 splits

# (e) 
# circuit, issue, petit, respon (some multiple)

# (f) 
pred_cons <- predict(cart_cons, newdata=supreme, type="prob")
ROCRpred_cons <- prediction(pred_cons[,2], supreme$unCons)
performance(ROCRpred_cons, "auc")@y.values
# AUC: 0.6519788

# (g) 
cart_lib <- rpart(as.factor(unLib) ~ petit+respon+circuit+unconst+lctdir+issue, data=supreme)
prp(cart_lib,extra=2)
print(cart_lib)
# 5 splits, variable at first level split is respon

# (h)
prp(cart_lib,extra=2)
# node with fewest observation is at the node where n=11
# here, 8 are 0. so 8 observations are not unanimous liberal decisions
# thus the percentage of unanimous liberal decisions here is 
3/11
# 0.2727273

# (i)
pred_cons <- predict(cart_cons, newdata=supreme, type="class")
pred_lib <- predict(cart_lib, newdata=supreme, type="class")
supreme$both_unanimous <- as.integer(pred_cons == 1 & pred_lib == 1)
table(supreme$both_unanimous)
# 2

# (j)
supreme$neither_unanimous <- as.integer(pred_cons == 0 & pred_lib == 0)
table(supreme$neither_unanimous)
# 502

# (k)
supreme_controversial <- subset(supreme, (both_unanimous == 1 | neither_unanimous == 1))
table(supreme_controversial$both_unanimous, supreme_controversial$neither_unanimous)
model_rehndir <- rpart(as.factor(rehndir) ~ petit+respon+circuit+unconst+lctdir+issue, data=supreme_controversial)
model_stevdir <- rpart(as.factor(stevdir) ~ petit+respon+circuit+unconst+lctdir+issue, data=supreme_controversial)
model_ocondir <- rpart(as.factor(ocondir) ~ petit+respon+circuit+unconst+lctdir+issue, data=supreme_controversial)
model_scaldir <- rpart(as.factor(scaldir) ~ petit+respon+circuit+unconst+lctdir+issue, data=supreme_controversial)
model_kendir <- rpart(as.factor(kendir) ~ petit+respon+circuit+unconst+lctdir+issue, data=supreme_controversial)
model_soutdir <- rpart(as.factor(soutdir) ~ petit+respon+circuit+unconst+lctdir+issue, data=supreme_controversial)
model_thomdir <- rpart(as.factor(thomdir) ~ petit+respon+circuit+unconst+lctdir+issue, data=supreme_controversial)
model_gindir <- rpart(as.factor(gindir) ~ petit+respon+circuit+unconst+lctdir+issue, data=supreme_controversial)
model_brydir <- rpart(as.factor(brydir) ~ petit+respon+circuit+unconst+lctdir+issue, data=supreme_controversial)

pred_rehndir <- predict(model_rehndir, newdata=supreme_controversial, type="class")
pred_stevdir <- predict(model_stevdir, newdata=supreme_controversial, type="class")
pred_ocondir <- predict(model_ocondir, newdata=supreme_controversial, type="class")
pred_scaldir <- predict(model_scaldir, newdata=supreme_controversial, type="class")
pred_kendir <- predict(model_kendir, newdata=supreme_controversial, type="class")
pred_soutdir <- predict(model_soutdir, newdata=supreme_controversial, type="class")
pred_thomdir <- predict(model_thomdir, newdata=supreme_controversial, type="class")
pred_gindir <- predict(model_gindir, newdata=supreme_controversial, type="class")
pred_brydir <- predict(model_brydir, newdata=supreme_controversial, type="class")


supreme_controversial$pred_rehndir <- ifelse(pred_rehndir==1, 1, 0)
supreme_controversial$pred_stevdir <- ifelse(pred_stevdir==1, 1, 0)
supreme_controversial$pred_ocondir <- ifelse(pred_ocondir==1, 1, 0)
supreme_controversial$pred_scaldir <- ifelse(pred_scaldir==1, 1, 0)
supreme_controversial$pred_kendir <- ifelse(pred_kendir==1, 1, 0)
supreme_controversial$pred_soutdir <- ifelse(pred_soutdir==1, 1, 0)
supreme_controversial$pred_thomdir <- ifelse(pred_thomdir==1, 1, 0)
supreme_controversial$pred_gindir <- ifelse(pred_gindir==1, 1, 0)
supreme_controversial$pred_brydir <- ifelse(pred_brydir==1, 1, 0)

supreme_controversial$combined_decision <- (supreme_controversial$pred_rehndir+supreme_controversial$pred_stevdir
                                               +supreme_controversial$pred_ocondir+supreme_controversial$pred_scaldir
                                               +supreme_controversial$pred_kendir+supreme_controversial$pred_soutdir
                                               +supreme_controversial$pred_thomdir+supreme_controversial$pred_gindir
                                               +supreme_controversial$pred_brydir)

supreme_controversial$majority_decision <- ifelse(supreme_controversial$combined_decision > 4, 1, 0)

confusion_matrix <- table(supreme_controversial$majority_decision, supreme_controversial$result)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy
# 0.7142857

# (l)
supreme$normal <- as.integer((pred_cons != 1 | pred_lib != 1) & (pred_cons != 0 | pred_lib != 0))
supreme$pred_unCons <- pred_cons
supreme$pred_unLib <- pred_lib

df_normal <- subset(supreme, normal==1)
df_normal$normal_prediction <- ifelse(df_normal$pred_unCons == 1, 1, 0)

confusion_matrix_normal <- table(df_normal$normal_prediction, df_normal$result)
confusion_matrix_normal
confusion_matrix_controversial <- table(supreme_controversial$majority_decision, supreme_controversial$result)
confusion_matrix_controversial

accurate <- sum(diag(confusion_matrix_normal)) + sum(diag(confusion_matrix_controversial))
total <- sum(confusion_matrix_normal) + sum(confusion_matrix_controversial)
total_accuracy <- accurate / total
total_accuracy
# 0.729097

# (m)
prp(cart_cons)
# based on cart_cons: 0 (not unanimous conservative decision)

prp(cart_lib)
# based on cart_lib: 1 (unanimous liberal decision)
# since the models are neither disagreeing nor both not unanimous, we predict unanimous liberal decision

# (n) random forest model
set.seed(1)
RF.1 <- randomForest(as.factor(result) ~ petit+respon+circuit+unconst+lctdir+issue, data=supreme)
pred.1 <- predict(RF.1, newdata=supreme)
cm.1 <- table(pred.1, supreme$result)
accuracy <- sum(diag(cm.1)) / sum(cm.1)
accuracy

# (o)
# the CART model is far more interpretable. we can make a prediction just based on the decision tree, as what we did in (o)
# however, the CART model is less accurate as seen in (n)

# on the contrary, the RF model is far more accurate but less interpretable.





































