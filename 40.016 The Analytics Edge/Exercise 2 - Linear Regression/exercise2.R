# Question 1
auto <- read.csv("Auto.csv")
head(auto)
summary(auto)
str(auto)

# Question 1a
auto$horsepower <- as.numeric(auto$horsepower) 
str(auto)
model1 <- lm(mpg~horsepower, data=auto)
summary(model1)

# Question 1b
print("Yes there is a strong relationship between the predictor and the response")
print("The relationship is negative as shown through the negative coefficient")

# Question 1c
predict.lm(model1, newdata=data.frame(horsepower=98), interval=("confidence"), level=0.99)

# Question 1d
summary(model1)
cor(auto$mpg, auto$horsepower, use="pairwise.complete.obs")
print("R-squared:")
cor(auto$mpg, auto$horsepower, use="pairwise.complete.obs")^2

# Question 1e
plot(auto$horsepower, auto$mpg)
abline(model1)

# Question 1f
layout(matrix(1:4,2,2)) 
plot(model1)
print("A good model would have randomly distributed residuals")
print("However, it is clear that there are patterns to the residuals, which presents evidence of non-linearity in the dataset which has not been captured")
print("The Q-Q plot is also not straight, indicating that the data might be non-linear especially in the extreme values")

# Question 6a
wine <- read.csv("winedata.csv")
head(wine)
str(wine)
wine$age91 <- 1991 - wine$vintage 
wine$age92 <- 1992 - wine$vintage 
head(wine)
# to compute the mean of wine at 91, for wines aged 15 yrs or older
mean(subset(wine$price91, age91>=15))
mean(wine$price91[wine$age91 >= 15])




# Question 6b
mean(subset(wine$price91, wine$hrain < mean(wine$hrain) & wine$tempdiff < mean(wine$tempdiff)))

# Question 6c
train <- subset(wine, wine$vintage <= 1981)
model1 <- lm(log(price91)~age91, data=train)
summary(model1)

# Question 6d 
confint(model1, level=0.99)

# Question 6e
test <- subset(wine, wine$vintage >= 1982)
predtest <- predict(model1, newdata=test)
predtest

truetest <- log(test$price91)
SSE <- sum((predtest - truetest)^2)
SST <- sum((truetest - mean(log(train$price91)))^2)
SSE
SST

testR2 <- 1 - SSE/SST
testR2

# Question 6f
plot(train$age91, log(train$price91))
abline(model1)

# Question 6g
model2 <- lm(log(price91)~age91+temp+hrain+wrain+tempdiff, data=train)
summary(model2)

# Question 6h
model3 <- lm(log(price91)~age91+temp+hrain, data=train)
summary(model3)

# Question 6i 
plot(train$age91, log(train$price91))

# Question 6l
model4 <- lm(log(price92)~temp+hrain+age92, data=train)
summary(model4)

# Question 6m 
# The p value of hrain is 0.320713, which is higher than the significant value of 0.1 
# Therefore, we should not reject the null hypothesis

# Question 6n
# Answer: second option

# Question 6o
# Answer: If only a few datapoints are missing, it's reasonable to drop them. 
# However, if most datapoints are missing, dropping all of it is not reasonable





# Question 7a
batters <- read.csv("batters.csv")
summary(batters)
str(batters)

batters$playerID[which.max(batters$salary)]

# Question 7b
max_salary2006 = max(batters$salary[batters$yearID == 2006])
min_salary2006 = min(batters$salary[batters$yearID == 2006])
max_salary2006 / min_salary2006 

# Question 7c
tapply(batters$salary[batters$yearID == 1996], batters$teamID[batters$yearID == 1996])
sort(tapply(batters$salary[batters$yearID == 1996], batters$teamID[batters$yearID == 1996]))
?tapply
batters$salary[batters$yearID == 1996]

# Question 7d
hist(batters$salary)
hist(log(batters$salary))

# Question 7e
model1 <- lm(log(salary) ~ R, batters)
summary(model1)

plot(batters$R, log(batters$salary))
abline(model1)

# Question 7f 
mean(log(batters$salary)[batters$R == 0], na.rm=T)
plot(batters$R, log(batters$salary))
abline(model1)
abline(h=mean(log(batters$salary)[batters$R == 0], na.rm=T))

# Question 7i
str(batters)
batters$OBP <- (batters$H + batters$BB + batters$HBP) / (batters$AB + batters$BB + batters$HBP + batters$SF)
batters$SLG <- (batters$H + batters$X2B + 2*batters$X3B +3*batters$HR) / batters$AB
mean(batters$OBP[batters$yearID==2006], na.rm=TRUE)

# Question j
t.test(batters$SLG[batters$yearID==1996], batters$SLG[batters$yearID==2006])


# Question k 
model2 <- lm(log(salary) ~ OBP+SLG, data=subset(batters, yearID==1996&AB>=130))
summary(model2)

# Question m
model3 <- lm(log(salary) ~ OBP+SLG, data=subset(batters, yearID==2006&AB>=130))
summary(model3)











