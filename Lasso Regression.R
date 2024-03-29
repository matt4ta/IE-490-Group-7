library(glmnet)

# Prepare data for model creation
BikeData <- readData()
X <- model.matrix(Rented_Bikes ~ ., BikeData)
Y <- BikeData$Rented_Bikes

# Create training and testing data sets
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

# calculate MSE
lasso.MSE <- mean((lasso.pred - Y[test]))^2
lasso.MSE


