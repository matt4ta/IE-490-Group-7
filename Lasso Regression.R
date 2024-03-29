library(glmnet)

BikeData <- readData()

# Only consider functioning days
# BikeData <- BikeData[BikeData$Functioning_Day == 1, ]
# BikeData$Functioning_Day <- NULL

# BikeData$Spring <- NULL
# BikeData$Summer <- NULL
# BikeData$Autumn <- NULL
# BikeData$Winter <- NULL

# Prepare data for model
X <- model.matrix(Rented_Bikes ~ ., BikeData)
Y <- BikeData$Rented_Bikes

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

# calculate MSE
lasso.MSE <- mean((lasso.pred - Y[test])^2)
lasso.MSE

# calculate R-squared
lasso.RSS <- sum((lasso.pred - Y[test])^2)
TSS <- sum(Y[test]^2)
lasso.r2 <- 1 - lasso.RSS / TSS

# calculate Adjusted R-squared
N <- dim(BikeData[test, ])[1]
p <- dim(BikeData[test, ])[2]
lasso.adjr2 <- 1 - (1 - lasso.r2^2) * (N - 1) / (N - p - 1)


#lasso.pred

# create linear model based on training data
lm.fit <- lm(Rented_Bikes ~ ., data = BikeData[train, ])

# apply model to validation set
lm.pred <- predict(lm.fit, BikeData[test, ])

#calculate linear model MSE
lm.MSE <- mean((lm.pred - Y[test])^2)

# calculate linear model r-squared
lm.RSS <- sum((lm.pred - Y[test])^2)
lm.r2 <- 1 - lm.RSS / TSS

# calculate linear model Adjusted R-Squared
lm.adjr2 <- 1 - (1 - lm.r2^2) * (N - 1) / (N - p - 1)


# output error values
lasso.MSE
lasso.adjr2
lm.MSE
lm.adjr2

