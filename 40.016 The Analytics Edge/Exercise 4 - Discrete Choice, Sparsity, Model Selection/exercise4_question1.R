# QUESTION 1
library(mlogit)
heating <- read.csv("Heating.csv")
str(heating)

# ic = installation cost
# oc = operational cost
# agehead = age of the household head
# dfidx format = wide

# gc = gas central
# gr = gas room
# ec = electric central
# er = electric room
# hp = heat pump

# choice = depvar

heating_idx <- dfidx(heating, choice="depvar", shape="wide", varying = c(3:12)) 
# 3 to 12 as varying because these are where the alternatives are located
# no indices required because the dataset has already provided it

# (A)
# (i)
model_1 <- mlogit(depvar ~ ic + oc -1, data=heating_idx)
summary(model_1)
# Yes, the coefficients are negative which are expected. 
# Price tends to discourage the selectino of a choice

# (ii) Yes, based on the low p value and the three stars 

# (iii) 
prediction_1 <- predict(model_1, newdata = heating_idx)
table(heating$depvar)/length(heating$depvar)
apply(prediction_1, 2, mean)

# (iv)
b_ic <- coef(model_1)["ic"]
b_oc <- coef(model_1)["oc"]
wtp <- b_oc / b_ic
wtp

# According to this model, the decision-makers are willing to pay
# $ 0.739 higher in installation cost for a $1 reduction in operating cost.
# It seems unreasonable for the decision-maker to pay only 74 cents
# higher for a one-time payment for a $1 reduction in annual costs.

# (B)
# (i)
r <- 0.12
heating$lcc.gc <- heating$ic.gc + heating$oc.gc/r
heating$lcc.gr <- heating$ic.gr + heating$oc.gr/r
heating$lcc.ec <- heating$ic.ec + heating$oc.ec/r
heating$lcc.er <- heating$ic.er + heating$oc.er/r
heating$lcc.hp <- heating$ic.hp + heating$oc.hp/r

heating_idx <- dfidx(heating, choice="depvar", shape="wide", varying = c(17:21)) 
model_2 <- mlogit(depvar ~ lcc -1, data=heating_idx)
summary(model_2)

logLik(model_1)
logLik(model_2)
# The log likelihood of model 2 is lower than model 1. 
# This indicates that the model is not as good. 

# (C)
heating_idx <- dfidx(heating, choice="depvar", shape="wide", varying = c(3:12)) 
model_3 <- mlogit(depvar ~ ic + oc, data=heating_idx, reflevel = "hp")
summary(model_3)

# (i) 
prediction_3 <- predict(model_3, newdata = heating_idx)
table(heating$depvar)/length(heating$depvar)
apply(prediction_3, 2, mean)
# The probabilities are very close
# In fact, for some variables, the probabilities are the same

# (ii) 
b_ic <- coef(model_3)["ic"]
b_oc <- coef(model_3)["oc"]
wtp_3 <- b_oc / b_ic
wtp_3
# The wtp value is 4.56, which makes more sense. 
# It makes sense that a customer would pay 4.56 more in installation cost
  # to get 1 dollar less in operational cost every year
# For example, if a person has to pay $45.6 more in installation cost,  
  # they could save $10 every year and if the system lasts for 5 years, 
  # they'd break even

# (iii)
summary(model_3)
heating_idx <- dfidx(heating, choice="depvar", shape="wide", varying = c(3:12)) 
model_4 <- mlogit(depvar ~ ic + oc, data=heating_idx, reflevel = "gr")
summary(model_4)

# model_3 
# (Intercept):ec  1.65884594  0.44841936  3.6993 0.0002162 ***
# (Intercept):er  1.85343697  0.36195509  5.1206 3.045e-07 ***
# (Intercept):gc  1.71097930  0.22674214  7.5459 4.485e-14 ***
# (Intercept):gr  0.30826328  0.20659222  1.4921 0.1356640    

# model_4
# (Intercept):ec  1.35058266  0.50715442  2.6631 0.0077434 ** 
# (Intercept):er  1.54517369  0.43298757  3.5686 0.0003588 ***
# (Intercept):gc  1.40271602  0.13398657 10.4691 < 2.2e-16 ***
# (Intercept):hp -0.30826328  0.20659222 -1.4921 0.1356640    
1.71097930 - 0.30826328

# The intercept for gr is 0.308 in model_3. 
# In model_4, gr is the reference level. So in the new model 
# all the alternative specific constants are reduced by 0.308. 
# Nothing else changes and the quality of fit remains unchanged.


# (D)
# (i)
heating_idx
heating_idx$ic_income <- heating_idx$ic / heating_idx$income 
model_5 <- mlogit(depvar ~ ic_income + oc, data=heating_idx, reflevel = "hp")
summary(model_5)
logLik(model_3) 
logLik(model_5)
# The loglikelihood of model_5 is actually lower than model_3. 
# So model_5 is not better than model_3. If anything, it is worse. 

# (ii)
model_6 <- mlogit(depvar ~ oc + ic | income, data = heating_idx)  
summary(model_6)
# gc = gas central
# gr = gas room
# ec = electric central
# er = electric room
# hp = heat pump

# All of the coefficients are negative which tells us that as income rises, 
# probability of choosing a heat pump increases relative to others, 
# The magnitude of the income coefficient for gr is the greatest 
# so we can infer that as income rises, 
# probability of choosing gas rooms drops relative to others. 

# The income terms are not significant


# (E)
heating_idx <- dfidx(heating, choice="depvar", shape="wide", varying = c(3:12)) 
model_7 <- mlogit(depvar ~ ic + oc, data=heating_idx)
summary(model_7)

P7 <- predict(model_7, newdata = heating_idx)
apply(P7, 2, mean)

# (i) 
heating1 <- heating
heating1$ic.hp <- 0.9 * heating1$ic.hp
heating_idx <- dfidx(heating1, choice="depvar", shape="wide", varying = c(3:12)) 
P7_1 <- predict(model_7, newdata = heating_idx)
apply(P7, 2, mean)
apply(P7_1, 2, mean)
# The rebate increases the market share of heat pumps by about 1 percent. 


# (ii) 
# ec = electric central
heating <- read.csv("Heating.csv")
# heating$ic.ec_new <- heating$ic.ec + 200 
# heating$oc.ec_new <- heating$oc.ec * 0.75
# str(heating)
# heating_idx <- dfidx(heating, choice="depvar", shape="wide", varying = c(3:12,17:18)) 
# 
# model_8 <- mlogit(depvar ~ ic + oc, data=heating_idx)
# summary(model_8)
# P8 <- predict(model_8, newdata = heating_idx)
# apply(P7, 2, mean)
# apply(P8, 2, mean)

str(heating)
df<- subset(heating, select = c(3:12))
df$ic.eci <- df$ic.ec + 200
df$oc.eci <- df$oc.ec * 0.75

# model_3 <- mlogit(depvar ~ ic + oc, data=heating_idx, reflevel = "hp")
df$hpexp<-exp(model_3$coefficients["oc"]*df$oc.hp+model_3$coefficients["ic"]*df$ic.hp)
df$ecexp<-exp(model_3$coefficients["oc"]*df$oc.ec+model_3$coefficients["ic"]*df$ic.ec+model_3$coefficients["(Intercept):ec"])
df$erexp<-exp(model_3$coefficients["oc"]*df$oc.er+model_3$coefficients["ic"]*df$ic.er+model_3$coefficients["(Intercept):er"])
df$gcexp<-exp(model_3$coefficients["oc"]*df$oc.gc+model_3$coefficients["ic"]*df$ic.gc+model_3$coefficients["(Intercept):gc"])
df$grexp<-exp(model_3$coefficients["oc"]*df$oc.gr+model_3$coefficients["ic"]*df$ic.gr+model_3$coefficients["(Intercept):gr"])
df$eciexp<-exp(model_3$coefficients["oc"]*df$oc.eci+model_3$coefficients["ic"]*df$ic.eci+model_3$coefficients["(Intercept):ec"])

df$sumexp <-apply(subset(df,select=c(13:17)),1,sum)
df$sumexpnew <-apply(subset(df,select=c(13:18)),1,sum)

# gc = gas central
# gr = gas room
# ec = electric central
# er = electric room
# hp = heat pump
# eci= ec, new technology

df$gc <- df$gcexp / df$sumexp
df$gr <- df$grexp / df$sumexp
df$ec <- df$ecexp / df$sumexp
df$er <- df$erexp / df$sumexp
df$hp <- df$hpexp / df$sumexp
df$eci<- df$eciexp/ df$sumexp

df$hpnew <-df$hpexp/df$sumexpnew
df$ecnew <-df$ecexp/df$sumexpnew
df$ernew <-df$erexp/df$sumexpnew
df$gcnew <-df$gcexp/df$sumexpnew
df$grnew <-df$grexp/df$sumexpnew
df$ecinew <-df$eciexp/df$sumexpnew

oldprob <- subset(df,select=c(21:25))
newprob <- subset(df,select=c(26:31))

marketshareold <- apply(oldprob,2,mean)
marketsharenew <- apply(newprob,2,mean)
marketshareold
marketsharenew











