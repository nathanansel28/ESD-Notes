# Question 6a
poll <- read.csv("AnonymityPoll.csv")
summary(poll)
str(poll)
print("How many people participated in the poll?")
print("1002, since there are 1002 observations")

# Question 6b
table(poll$Smartphone)
summary(poll$Smartphone)
print("487 uses smartphone, 472 don't use smartphones")
sum(is.na(poll$Smartphone))
print("43 entries are NA")

# Question 6c
table(poll$Smartphone, poll$Internet.Use)
print("186 uses neither a smartphone nor the internet")
print("17 uses a smartphone but no internet")
print("285 uses the internet but no smartphone")
print("470 use both smartphone and internet")

# Question 6d
sum(is.na(poll$Smartphone))
sum(is.na(poll$Internet.Use))

# Question 6e
limited <- subset(poll, poll$Internet.Use == 1 | poll$Smartphone == 1) 
str(limited)
print("792 people are in the new dataframe")

# Question 6f
sapply(limited, function(x) any(is.na(x)))

# Question 6g
print(mean(limited$Info.On.Internet))

# Question 6h
sum(limited$Info.On.Internet == 0)
sum(limited$Info.On.Internet == 11)
table(limited$Info.On.Internet)

# Question 6i
summary(limited$Worry.About.Info)
mean(limited$Worry.About.Info, na.rm=TRUE) #NA needs to be removed
print("Since the mean is 0.4886, the proportion of people who worry about their info is 0.4886")

# Question 6j
summary(limited$Anonymity.Possible)
mean(limited$Anonymity.Possible, na.rm=TRUE)

# Question 6k
hist(limited$Age, breaks=20)
print("The best represented people are those around 60 years of age")

# Question 6l
table(limited$Info.On.Internet, limited$Age)
max(table(limited$Info.On.Internet, limited$Age))
plot(limited$Info.On.Internet)

# Question 6m
plot(jitter(c(1,2,3)))

# Question 6n
plot(limited$Age, limited$Info.On.Internet)
plot(jitter(limited$Age), jitter(limited$Info.On.Internet))

# Question 6o
tapply(limited$Info.On.Internet, limited$Smartphone, mean) # calculate the mean if smartphone == 1 vs if smartphone == 0

# Question 6p
tapply(limited$Tried.Masking.Identity, limited$Smartphone, table)
tapply(limited$Tried.Masking.Identity, limited$Smartphone, summary)
print("0.1174 of people without smartphone tried masking identity")
print("0.1925 of people with smartphone tried masking identity")



