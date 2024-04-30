library(gam)
setwd("~/GitHub/IE-490-Group-7")
source('ReadData.R')
X <- readData()
# X$Visibility <- NULL
# Prepare data for model
# Y <- BikeData$Rented_Bikes

N <- dim(X)[1]
p <- dim(X)[2]

# gam.fit <- gam(Y ~ s(Hour, 4) + ns(Temp, 4) + lo(Humidity, 0.3) + 
#                  lo(Wind_speed, 0.3) + ns(Visibility, 4) + s(Solar_Radiation) + 
#                  s(Rainfall) + s(Snowfall) + Winter + Spring + Summer + Autumn + 
#                  Holiday + WeekdaySun + WeekdayMon + WeekdayTues + WeekdayWed + 
#                  WeekdayThur + WeekdaySat, data = X)

# gam.fit <- gam(Rented_Bikes ~ s(Hour) + s(Temp) + s(Humidity) + 
#                  s(Wind_speed) + s(Visibility) + s(Solar_Radiation) + 
#                  s(Rainfall) + s(Snowfall) + Holiday + Seasons + Weekday, 
#                data = X)

gam.fit <- gam(Rented_Bikes ~ s(Hour, 7) + s(Temp, 5) + lo(Humidity, 3) + 
                s(Wind_speed, 3) + s(Visibility, 3) + s(Solar_Radiation, 4) + 
                s(Rainfall, 4) + s(Snowfall, 2) + Holiday + Seasons + Weekday, 
                data = X)

plot(gam.fit, se = T, col = "blue")


# perform k-fold cross validation
k <- 5
gam.cv.errors <- rep(0, k)
set.seed(1)
indices <- sample(N, N)
fold.size <- N / k


# k-fold CV for GAM -------------------------------------------------------

RMSE.df <- rep(0, 20)
for (j in 1:20) {
  gam.cv.errors <- rep(0, k)
  for (i in 1:k) {
    validation <- indices[(1 + (i - 1) * fold.size):(i * fold.size)]
    train.X <- X[-validation,]
    validation.X <- X[validation,]
    train.Y <- Y[-validation]
    validation.Y <- Y[validation]
    
    gam.cv <- gam(Rented_Bikes ~ s(Hour, j) + s(Temp, 5) + s(Humidity, 3) + 
                    ns(Wind_speed, 3) + s(Visibility, 3) + s(Solar_Radiation, 4) + 
                    s(Rainfall, 4) + s(Snowfall, 2) + Holiday + Seasons, 
                  data = train.X)
    gam.cv.pred <- predict(gam.cv, newdata = validation.X)
    gam.cv.errors[i] <- sqrt(mean((gam.cv.pred - validation.Y)^2))
  }

  RMSE.df[j] <- mean(gam.cv.errors)
}

plot(1:20, RMSE.df, type = "b", 
     xlab = "Degrees of Freedom of Smoothing Splines for Hours",
     ylab = "RMSE")

diffs <- rep(0, 18)

for(m in 3:20) {
  diffs[m - 2] = (RMSE.df[m - 1] - RMSE.df[m]) / (RMSE.df[m - 2] - RMSE.df[m - 1])
}
plot(1:18, diffs, type = "b")

# seems like df = 5 is best
