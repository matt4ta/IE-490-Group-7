# reads data from csv and into a data frame
readData <- function() {
  setwd("~/GitHub/IE-490-Group-7")
  data <- read.csv('SeoulBikeData.csv', encoding = "latin1")
  
  # add weekday for each observation
  Weekday <- as.POSIXlt(data$Date, format = "%d/%m/%Y")$wday
  Weekday <- c("Sun", "Mon", "Tues", "Wed", "Thur", "Fri", "Sat")[as.POSIXlt(data$Date, format = "%d/%m/%Y")$wday + 1]
  data <- cbind(data, Weekday)
  
  # remove date
  data$Date <- NULL
  
  # remove observations for non-functioning days
  data <- data[data$Functioning_Day == 1, ]
  data$Functioning_Day <- NULL
  
  # remove observations with a humidity of 0%
  data <- data[data$Humidity > 0, ]
  
  # Removing qualitative data which has already been cleaned
  # data$Seasons <- NULL 
  data$Holiday_Orig <- NULL
  data$Functioning_Day_Orig <- NULL
  
  # Remove dew point temp  and humidity as a predictor due to collinearity with temperature
  data$Dew_Point_Temp <- NULL
  data$Humidity <- NULL
  
  data$Spring <- NULL
  data$Summer <- NULL
  data$Autumn <- NULL
  data$Winter <- NULL
  
  return(data)
}


data <- readData()

