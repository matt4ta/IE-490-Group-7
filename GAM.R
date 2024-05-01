library(gam)
setwd("~/GitHub/IE-490-Group-7")
source('ReadData.R')
X <- readData()
# X$Visibility <- NULL
# Prepare data for model
Y <- X$Rented_Bikes

N <- dim(X)[1]
p <- dim(X)[2]

gam.fit <- gam(Rented_Bikes ~ s(Hour, 8) + s(Temp, 5) + s(Humidity, 3) + 
                 ns(Wind_speed, 3) + s(Visibility, 3) + ns(Solar_Radiation, 6) + 
                 s(Rainfall, 4) + Snowfall + Holiday + Seasons + Weekday,  
                data = X)
summary(gam.fit)
plot(gam.fit, se = T, col = "blue", terms = labels.Gam(X$Hour))

plot(s(X$Hour, 8), se = T)

# k-fold cv to choose parameters ------------------------------------------

k <- 5
gam.cv.errors <- rep(0, k)
set.seed(1)
indices <- sample(N, N)
fold.size <- N / k

gam.cv.errors <- rep(0, k)
gam.cv.variance <- rep(0, k)
gam.cv.bias <- rep(0,k)

for (i in 1:k) {
  validation <- indices[(1 + (i - 1) * fold.size):(i * fold.size)]
  train.X <- X[-validation,]
  validation.X <- X[validation,]
  train.Y <- Y[-validation]
  validation.Y <- Y[validation]
  
  gam.cv <- gam(Rented_Bikes ~ s(Hour, 8) + s(Temp, 5) + s(Humidity, 3) + 
                  ns(Wind_speed, 3) + s(Visibility, 3) + ns(Solar_Radiation, 6) + 
                  s(Rainfall, 4) + Snowfall + Holiday + Seasons + Weekday, 
                data = train.X)
  gam.cv.pred <- predict(gam.cv, newdata = validation.X)
  gam.cv.errors[i] <- sqrt(mean((gam.cv.pred - validation.Y)^2))
  gam.cv.variance[i] <- mean((gam.cv.pred - mean(gam.cv.pred))^2)
  gam.cv.bias[i] <- mean(gam.cv.pred - mean(validation.Y))
}

GAM.RMSE <- mean(gam.cv.errors)
GAM.variance <- mean(gam.cv.variance)
GAM.bias <- mean(gam.cv.bias)

GAM.RMSE
GAM.variance
GAM.bias

# k-fold CV to find df of hours for GAM -------------------------------------------------------

RMSE.df <- rep(0, 25)
variance.df <- rep(0, 25)
bias.df <- rep(0, 25)
for (j in 1:25) {
  gam.cv.errors <- rep(0, k)
  gam.cv.variance <- rep(0, k)
  gam.cv.bias <- rep(0,k)

  for (i in 1:k) {
    validation <- indices[(1 + (i - 1) * fold.size):(i * fold.size)]
    train.X <- X[-validation,]
    validation.X <- X[validation,]
    train.Y <- Y[-validation]
    validation.Y <- Y[validation]
    
    gam.cv <- gam(Rented_Bikes ~ s(Hour, j) + s(Temp, 5) + s(Humidity, 3) + 
                    ns(Wind_speed, 3) + s(Visibility, 3) + s(Solar_Radiation, 4) + 
                    s(Rainfall, 4) + Snowfall + Holiday + Seasons + Weekday, 
                  data = train.X)
    gam.cv.pred <- predict(gam.cv, newdata = validation.X)
    gam.cv.errors[i] <- sqrt(mean((gam.cv.pred - validation.Y)^2))
    gam.cv.variance[i] <- mean((gam.cv.pred - mean(gam.cv.pred))^2)
    gam.cv.bias[i] <- mean(gam.cv.pred - mean(validation.Y))
  }

  RMSE.df[j] <- mean(gam.cv.errors)
  variance.df[j] <- mean(gam.cv.variance)
  bias.df[j] <- mean(gam.cv.bias)
}

plot(1:25, scale(RMSE.df), type = "b", 
     xlab = "Degrees of Freedom of Smoothing Splines for Hours",
     ylab = "Standardized Values")

lines(scale(variance.df), type = "b", col = 'blue')

lines(scale(bias.df), type = "b",  col = 'red')
legend('topright', legend = c('RMSE', "Variance", "Bias"), 
       col = c('black', 'blue', 'red'),
       lty = 1, lw = 2)
grid()





diffs <- rep(0, 18)

for(m in 3:20) {
  diffs[m - 2] = (RMSE.df[m - 1] - RMSE.df[m]) / (RMSE.df[m - 2] - RMSE.df[m - 1])
}
plot(1:18, diffs, type = "b")

# seems like df = 5 is best
