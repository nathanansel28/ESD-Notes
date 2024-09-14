# Question 1
baseballlarge <- read.csv("baseballlarge.csv")
head(baseballlarge)
str(baseballlarge)

# Question 1a
nrow(baseballlarge)
table(baseballlarge$Year)
baseballlarge <- subset(baseballlarge, baseballlarge$Playoffs==1)
str(baseballlarge)
table(baseballlarge$Year)
table(table(baseballlarge$Year))
length(unique(baseballlarge$Team))

# Question 1b
baseballlarge$NumCompetitors <- table(baseballlarge$Year)[as.character(baseballlarge$Year)]
table(baseballlarge$NumCompetitors)
table(baseballlarge$NumCompetitors)["8"]
n_teams <- subset(baseballlarge, baseballlarge$NumCompetitors==8)
str(n_teams)

# Question 1c
baseballlarge$WorldSeries <- as.integer(baseballlarge$RankPlayoffs == 1)
table(baseballlarge$WorldSeries)

# Question 1d


model1 <- glm(WorldSeries ~ Year, family=binomial, data=baseballlarge)
summary(model1) # AIC: 232.35
model2 <- glm(WorldSeries ~ RS, family=binomial, data=baseballlarge)
summary(model2) # AIC: 241.45
model3 <- glm(WorldSeries ~ RA, family=binomial, data=baseballlarge)
summary(model3) # AIC: 237.88
model4 <- glm(WorldSeries ~ W, family=binomial, data=baseballlarge)
summary(model4) # AIC: 239.51
model5 <- glm(WorldSeries ~ OBP, family=binomial, data=baseballlarge)
summary(model5) # AIC: 242.02
model6 <- glm(WorldSeries ~ SLG, family=binomial, data=baseballlarge)
summary(model6) # AIC: 239.23
model7 <- glm(WorldSeries ~ BA, family=binomial, data=baseballlarge)
summary(model7) # AIC: 243.08
model8 <- glm(WorldSeries ~ RankSeason, family=binomial, data=baseballlarge)
summary(model8) # AIC: 238.75
model9 <- glm(WorldSeries ~ NumCompetitors, family=binomial, data=baseballlarge)
summary(model9) # AIC: 230.96
model10 <- glm(WorldSeries ~ League, family=binomial, data=baseballlarge)
summary(model10) # AIC: 242.88

# Question 1e
modelsig <- glm(WorldSeries ~ Year+RA+RankSeason+NumCompetitors+W+SLG, data=baseballlarge, family=binomial)
summary(modelsig)
p_val <- summary(modelsig)$coefficients[,4]
p_val

# Question 1f
cor(baseballlarge[,c("Year", "RA", "RankSeason", "NumCompetitors", "W", "SLG")])


# Question 1g
modelg1 <- glm(WorldSeries~Year+RA, data=baseballlarge, family=binomial)
summary(modelg1) #AIC: 233.88
modelg2 <- glm(WorldSeries~Year+RankSeason, data=baseballlarge, family=binomial)
summary(modelg2) #AIC: 233.55
modelg3 <- glm(WorldSeries~Year+NumCompetitors, data=baseballlarge, family=binomial)
summary(modelg3) #AIC: 232.9
modelg4 <- glm(WorldSeries~RA+RankSeason, data=baseballlarge, family=binomial)
summary(modelg4) #AIC: 238.22
modelg5 <- glm(WorldSeries~RA+NumCompetitors, data=baseballlarge, family=binomial)
summary(modelg5) #AIC: 232.74
modelg6 <- glm(WorldSeries~RankSeason+NumCompetitors, data=baseballlarge, family=binomial)
summary(modelg6) #AIC: 232.52


# Question 2a
Parole <- read.csv("parole.csv")
str(Parole)

# Question 2b
table(Parole$Violator)

# Question 2c
table(Parole$State)
table(Parole$Crime)

# Question 2d
set.seed(144)
library(caTools)
split <- sample.split(Parole$Violator, SplitRatio = 0.7)
train <- subset(Parole, split == TRUE)
test <- subset(Parole, split == FALSE)
split <- sample.split(Parole$Violator, SplitRatio = 0.7)
train1 <- subset(Parole, split == TRUE)
test1 <- subset(Parole, split == FALSE)
identical(train, train1)
identical(test, test1)

# Question 2e
model1 <- glm(Violator~., data=train, family=binomial)
summary(model1)
coef_table <- summary(model1)$coefficients
coef_table
p_val <- coef_table[,4]
p_val
sig_vars <- names(p_val[p_val<=0.05])
sig_vars

model2 <- glm(Violator ~ RaceWhite + State + MultipleOffenses, data=train, family=binomial)
summary(model2)

# Question 2f
# Correct statement: 4

# Question 2g
model1$coef
logodds <- model1$coeff[1] + model1$coef[2]*1 + model1$coef[3]*1 + model1$coef[4]*50 + model1$coef[6]*1 +model1$coef[8]*3 + model1$coef[9]*12 + model1$coef[12]*1
logodds
odds <- exp(logodds)
odds
prob <- odds/(1+odds)
prob
prob <- exp(logodds)/(1+exp(logodds))
prob

# Question 2h
pred <- predict(model1, newdata=test, type="response")
max(pred)

# Question 2i
pred_table <- table(pred > 0.5, test$Violator)
pred_table

accuracy <- sum(diag(pred_table[1,1]))/sum(pred_table)
accuracy

# Question 2j
table(test$Violator)[1]/nrow(test)

# Question 2k
# 2nd

# Question 2l
summary(model1)


















