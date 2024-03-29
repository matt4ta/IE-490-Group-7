#Colinearity Test
library(caTools)
library(car)
library(quantmod)
library(MASS)
library(corrplot)
library(readxl)
dataS <- read_excel("C:/Users/Chris/Desktop/490/SeoulBikeData.xlsx")

head(data)
model_all <- lm(Rented_Bikes ~ ., data = dataS)  # with all the independent variables in the dataframe

summary(model_all)
vif(model_all)
vif_values <- vif(model_all)           #create vector of VIF values

barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 5, lwd = 3, lty = 2)    #add vertical line at 5 as after 5 there is severe correlation
data_x <- dataS[,2:10]                                       # independent variables 

var <- cor(data_x)                                         # independent variables correlation matrix 

var_inv <- ginv(var)                                       # independent variables inverse correlation matrix 

colnames(var_inv) <- colnames(data_x)                      # rename the row names and column names
rownames(var_inv) <- colnames(data_x)

corrplot(var_inv,method='number',is.corr = F)              # visualize the multicollinearity

#Linear and multiple linear regression
library(ISLR2)
library(boot)
library(readxl)
library(ggplot2)
#loading csv
bike_data <- read_excel("C:/Users/Chris/Desktop/490/SeoulBikeData.xlsx")

# Multiple Linear Regression
lm.fit <- lm(Rented_Bikes ~ ., data = bike_data)
summary(lm.fit)
predicted_values <- predict(lm.fit)

# Step 3: Calculate Mean Squared Error (MSE)
mse <- mean((bike_data$Rented_Bikes - predicted_values)^2)

# Print the Mean Squared Error
print(mse)
rsquared <- summary(lm.fit)$r.squared
rsquared
