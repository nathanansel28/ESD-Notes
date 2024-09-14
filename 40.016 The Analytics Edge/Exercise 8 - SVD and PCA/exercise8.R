# ============
#  QUESTION 1
# ============
# (a) 
# -------------------
# Extracting the data
# -------------------
lsi_matrix <- read.table("lsiMatrix.txt")
X <- as.matrix(t(lsi_matrix))
X
#       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
# V1      2    0    4    0    0    0    0    0    0
# V2      0    0    2    0    0    0    0    0    0
# V3      0    0    0    3    0    0    0    0    0
# V4      0    0    0    0    0    0    2    0    0
# V5      0    0    0    0    0    0    0    2    0
# V6      0    0    2    0    0    0    0    0    0
# V7      0    0    2    0    0    0    0    0    0
# V8      2    0    0    0    0    0    0    0    0
# V1, V2, represents the words
# [,1], [,2] represents the documents

dim(X)
# 460   9
# 460 unique words in a set of 9 documents 

# ----------------------
# Performing SVD
# ----------------------
m <- svd(X)
m$u
dim(m$u)
# 460   9
m$v
dim(m$v)
# 9 9

# ----------------------
# Low-rank approximation
# ----------------------
mreduced <- m$u[,1:2] %*% diag(m$d[1:2]) %*% t(m$v[,1:2])
mreduced
dim(mreduced)
# [1] 460   9

plot(m$v[,1],m$v[,2])
text(m$v[,1],m$v[,2],c(1:9),adj=2)
# Documents 7 and 8 seem very close to each other.

# (b)
# X = USV^T is the SVD
# USq_hat = q 
# q_hat = S^{-1}U^-{1}q 

words <- readLines("lsiWords.txt")
which(words=="abducted") # 23
q <- matrix(0,nrow=460,ncol=1)
q[23] <- 1
dim(q)

# In an SVD object in R
# d : a vector containing the singular values of x
# u : a matrix whose columns contain the left singular vectors of x
# v : a matrix whose columns contain the right singular vectors of x
q_hat <- solve(diag(m$d[1:2])) %*% t(m$u[,1:2]) %*% q
str(q_hat)

cosine <- matrix(0,9,1)
for(i in 1:9){
  cosine[i] <- sum(q_hat*m$v[i,1:2])/sqrt(sum(q_hat^2)*sum(m$v[i,1:2]^2))
}
order(cosine,decreasing=TRUE)
# Documents 1, 3, and then 2 are closest to q


# ============
#  QUESTION 2
# ============
# (a)
str(iris)
# 150 observations
# 5 variables

# (b)
iris_data <- subset(iris, select=-c(Species))
iris_sp <- iris$Species

# (c) 
pairs(iris_data)
cor(iris_data)
sort(cor(iris_data), decreasing=TRUE)
# petal width and petal length 
# petal length and sepal length
# petal width and sepal length

# (d) (i)
PCA <- prcomp(iris_data, scale=F)
print(PCA)
PCA$rotation
PCA$x[1:10, ]
plot(PCA, type="l", main="Scree plot")
PCA.ns <- PCA$sdev^2 / sum(PCA$sdev^2)
plot(cumsum(PCA.ns), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')+abline(h=0.9,col="red")
PCA.ns[1]
# Using just 1 principal component is enough to explain over 90% of the variance

# (d) (ii)
library(factoextra)
# Graph of individuals
fviz_pca_ind(PCA, label = "var", habillage=iris_sp)
fviz_pca_ind(PCA, label = "var", habillage=iris_sp,addEllipses=TRUE, ellipse.level=0.95)

# Biplot of individuals and variables
fviz_pca_biplot(PCA, label = "var", habillage=iris_sp)
fviz_pca_biplot(PCA, label = "var", habillage=iris_sp,addEllipses=TRUE, ellipse.level=0.95)

# (e)
PCA.scaled <- prcomp(iris_data, scale=TRUE)
print(PCA.scaled)
PCA.scaled.ns <- PCA.scaled$sdev^2 / sum(PCA.scaled$sdev^2)
plot(cumsum(PCA.scaled.ns), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')+abline(h=0.9,col="red")
PCA.scaled.ns[1]
sum(PCA.scaled.ns[1:2])
# Using just 1 principal component is NOT enough to explain over 90% of the variance
# We now need 2 principal components

# Graph of individuals
fviz_pca_ind(PCA.scaled, label = "var", habillage=iris_sp)
fviz_pca_ind(PCA.scaled, label = "var", habillage=iris_sp,addEllipses=TRUE, ellipse.level=0.95)

# Biplot of individuals and variables
fviz_pca_biplot(PCA.scaled, label = "var", habillage=iris_sp)
fviz_pca_biplot(PCA.scaled, label = "var", habillage=iris_sp,addEllipses=TRUE, ellipse.level=0.95)

# In the previous model, the petal length on average has a high value and was thus assigned a 
# higher contribution. However, scaling reduces and corrects the effect of this variable on the PC 


# ============
#  QUESTION 3
# ============
mroz  <- read.csv("mroz.csv")
str(mroz )

# (a)
mroz$exper2 <- mroz $exper ** 2
mroz.lm <- lm(hours ~ ., data=mroz)
summary(mroz.lm)
# R-squared: 0.2656
# Adjusted R-squared: 0.2587

# Fitted equation: 
# hours = 1330.4824 -442.0899(kidslt6) -32.7792(kidsge6) -30.5116(age) 
# + 28.7611(educ) + 65.6725(exper) -3.4466(nwifeinc) -0.7005(exper2)

# exper2 is significant but not as significant as exper
# experience has a diminishing marginal effect on wage, as the coefficient of exper2 is negative

# (b)
mroz.pred <- predict(mroz.lm, newdata=mroz)
mroz.pred
mroz.pred.negative <- subset(mroz.pred, mroz.pred < 0)
mroz.pred.zero <- subset(mroz.pred, mroz.pred == 0)
length(mroz.pred.negative)
length(mroz.pred.zero)
# there are 39 negative fitted values, and 0 zero fitted values. 

# (c)
library(survival)
mroz.tobit <- survreg(Surv(hours, hours > 0, type="left") ~ ., data=mroz, dist="gaussian")
summary(mroz.tobit)
# The signs of all the coefficients match

# (d)
mroz.tobit.beta.x <- predict(mroz.tobit, newdata=mroz)
mroz.tobit.predict <- ((mroz.tobit.beta.x * pnorm(mroz.tobit.beta.x / mroz.tobit$scale)) 
                       + (mroz.tobit$scale * dnorm(mroz.tobit.beta.x / mroz.tobit$scale)))
cor(mroz.pred, mroz$hours)^2
cor(mroz.tobit.predict, mroz$hours)^2
# The tobit model indicates a slightly better performance

# (e)
kidslt6 <- mean(mroz$kidslt6)
kidsge6 <- mean(mroz$kidsge6)
age <- mean(mroz$age)
educ <- mean(mroz$educ)
exper <- mean(mroz$exper)
nwifeinc <- mean(mroz$nwifeinc)
exper2 <- mean(mroz$exper2)

lm_hours_int <- (1330.4824 -442.0899*(kidslt6) -32.7792*(kidsge6) -30.5116*(age) 
             + 65.6725*(exper) -3.4466*(nwifeinc) -0.7005*(exper2))
lm_hours_int
lm_tobit_int <- (965.3053 -894.0217*(kidslt6) -16.2180*(kidsge6) -54.4050*(age) 
             + 131.5643*(exper) -8.8142*(nwifeinc) -1.8642*(exper2))
lm_tobit_int

# linear model
# hours = 387.1937 + 28.7611 * (educ)
# tobit model
# hours = -694.1215 + 80.6456 * (educ)    



lm_const <- (mroz.lm$coef[1] + mroz.lm$coef[2]*mean(mroz$kidslt6) + mroz.lm$coef[3]*mean(mroz$kidsge6) 
           + mroz.lm$coef[4]*mean(mroz$age) + mroz.lm$coef[6]*mean(mroz$exper) 
           + mroz.lm$coef[7]*mean(mroz$nwifeinc) + mroz.lm$coef[8]*mean(mroz$exper2))
lm_const + mroz.lm$coef[5]*8
lm_const + mroz.lm$coef[5]*12
(lm_const + mroz.lm$coef[5]*12) - (lm_const + mroz.lm$coef[5]*8)
# 617.2817 
# 732.3262 
# difference: 115.0445 

tobit_const <- (mroz.tobit$coef[1] + mroz.tobit$coef[2]*mean(mroz$kidslt6) + mroz.tobit$coef[3]*mean(mroz$kidsge6) 
                + mroz.tobit$coef[4]*mean(mroz$age) + mroz.tobit$coef[6]*mean(mroz$exper) 
                + mroz.tobit$coef[7]*mean(mroz$nwifeinc) + mroz.tobit$coef[8]*mean(mroz$exper2))
tobit_hours_8 <- (((mroz.tobit$coef[5]*8+tobit_const) * pnorm(((mroz.tobit$coef[5]*8+tobit_const))/mroz.tobit$scale)) 
                  + (mroz.tobit$scale*dnorm(((mroz.tobit$coef[5]*8+tobit_const))/mroz.tobit$scale)))
tobit_hours_8
tobit_hours_12 <- (((mroz.tobit$coef[5]*12+tobit_const) * pnorm(((mroz.tobit$coef[5]*12+tobit_const))/mroz.tobit$scale)) 
                  + (mroz.tobit$scale*dnorm(((mroz.tobit$coef[5]*12+tobit_const))/mroz.tobit$scale)))
tobit_hours_12
tobit_hours_12 - tobit_hours_8
# 423.5725 
# 597.6833 
# difference: 174.1108 

# The difference in the number of hours in the tobit model is higher than in the linear model
# As such, there is an increasing marginal effect of education on the hours worked in the tobit model



















































