library(glmnet)
setwd("~/GitHub/IE-490-Group-7")
source('ReadData.R')
BikeData <- readData()

# Only consider functioning days
# BikeData <- BikeData[BikeData$Functioning_Day == 1, ]
# BikeData$Functioning_Day <- NULL


# Prepare data for model
X <- model.matrix(Rented_Bikes ~ ., BikeData)
Y <- BikeData$Rented_Bikes

N <- dim(X)[1]
p <- dim(X)[2]

# Create training and validation data sets
set.seed(1)
train <- sample(1:nrow(X), nrow(X) * 4 / 5)
test = (-train)

# Find best lambda value using Cross-Validation
cv.out.lasso <- cv.glmnet(X[train, ], Y[train], alpha = 1) 
plot(cv.out.lasso)
bestlam <- cv.out.lasso$lambda.min

# Create lasso regression model with best lambda value
lasso.mod <- glmnet(X[train, ], Y[train], alpha = 1, lambda = bestlam)
lasso.mod$beta

# Apply model to validation set
lasso.pred <- predict(lasso.mod, s = bestlam, newx = X[test, ], type = "response")

num.folds <- 5
lasso.cv.errors <- rep(0, num.folds)
set.seed <-(1)
indices <- sample(N, N)
fold.size <- N / num.folds

for (i in 1:num.folds) {
  validation <- indices[(1 + (i - 1) * fold.size):(i * fold.size)]
  train.X <- X[-validation,]
  validation.X <- X[validation,]
  train.Y <- Y[-validation]
  validation.Y <- Y[validation]
  
  lasso.cv <- glmnet(train.X, train.Y, alpha = 1, lambda = bestlam)
  lasso.cv.pred <- predict(lasso.cv, s = bestlam, newx = validation.X, type = "response")
  lasso.cv.errors[i] <- sqrt(mean((lasso.cv.pred - validation.Y)^2))
}

lasso.RMSE <- mean(lasso.cv.errors)
lasso.RMSE



