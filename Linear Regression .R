##### Simple Linear Regression

## Libraries
library(MASS)
library(ISLR2)

## Simple Linear Regression

###
?Boston
head(Boston)
###
lm.fit <- lm(medv ~ lstat)
?lm
###
lm.fit <- lm(medv ~ lstat, data = Boston)
attach(Boston)
lm.fit <- lm(medv ~ lstat)
###
lm.fit
summary(lm.fit)
###
names(lm.fit)
coef(lm.fit)
###
confint(lm.fit)
###
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))),
        interval = "confidence")
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))),
        interval = "prediction")
###
plot(lstat, medv)
abline(lm.fit)
###
abline(lm.fit, lwd = 3)
abline(lm.fit, lwd = 3, col = "red")
plot(lstat, medv, col = "red")
plot(lstat, medv, pch = 20)
plot(lstat, medv, pch = "+")
plot(1:20, 1:20, pch = 1:20)
###
par(mfrow = c(2, 2))
plot(lm.fit)

###### Linear Regression

# We are going to use the advertising data

# First, we need to load the data from the .csv file
Advertising <- read.csv("Advertising.csv")
View(Advertising)

# For illustration purposes, we will create X and y
X <- Advertising[1:3] # 200x3 matrix containing input variables
y <- Advertising[4]   # 200x1 vector containing output variable
X <- cbind(1,X)       # we attach a 200x1 vector of 1s to X to account for the intercept
colnames(X)[1] <- "Intercept"
head (X)
head(y)

# Finding the optimal coefficients
# Matrix form
X <- as.matrix(X)
y <- as.matrix(y)
beta_matrix = solve(t(X)%*%X,t(X)%*%y)
beta_matrix
# lm() function
lm.fit <- lm(sales ~ TV + radio + newspaper, data = Advertising)
beta_lm <- lm.fit$coefficients
beta_lm
summary(lm.fit)

# Prediction
new_x <- cbind.data.frame(50,50,50)
names(new_x) <- c("TV", "radio", "newspaper")
predict(lm.fit, new_x,
        interval = "confidence")
predict(lm.fit, new_x,
        interval = "prediction")

# Is the relationship linear?
par(mfrow = c(2, 2))
plot(lm.fit)
# Data transformation
log_sales <- log(Advertising$sales)
Advertising <- cbind.data.frame(Advertising, log_sales)
lm.fit.log <- lm(log_sales ~ TV + radio + newspaper, data = Advertising)
summary(lm.fit.log)
par(mfrow = c(2, 2))
plot(lm.fit.log)

# Is there synergy?
lm.fit.syn <- lm(sales ~ TV + radio + TV*radio, data = Advertising)
summary(lm.fit.syn)


##### Multiple Linear Regression - Boston Data Set

# Multiple Linear Regression
lm.fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)

X <- cbind(1, Boston$lstat, Boston$age)
y <- Boston$medv
beta_hat <- solve(t(X)%*%X)%*%t(X)%*%y

lm.fit <- lm(medv ~ ., data = Boston)
summary(lm.fit)

library(car)
vif(lm.fit)

lm.fit <- lm(medv ~ . - age, data = Boston)
summary(lm.fit)

# Interaction terms
summary(lm(medv ~ lstat*age, data = Boston))

# Non-linear transformations of the predictors
plot(Boston$lstat, Boston$medv)
lm.fit2 <- lm(medv ~ lstat + I(lstat^2), data = Boston)
summary(lm.fit2)

lm.fit <- lm(medv ~ lstat, data = Boston)
anova(lm.fit,lm.fit2)

par(mfrow = c(2,2))
plot(lm.fit2)

lm.fit5 <- lm(medv ~ poly(lstat, 5), data = Boston)
summary(lm.fit5)

summary(lm(medv ~ log(rm), data = Boston))

# Qualitative Predictors
head(Carseats)
lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)
contrasts(Carseats$ShelveLoc)

# Writing functions
LoadLibraries <- function()
{
  library(MASS)
  library(ISLR2)
  print("The libraries have been loaded")
}
