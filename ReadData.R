# reads data from csv and puts it into a matrix model thing
readData <- function() {
  setwd("~/GitHub/IE-490-Group-7")
  data <- read.csv('SeoulBikeData.csv', encoding = "latin1")
  data$Date <- NULL # model.matrix was making Date a qualitative variable so I took it out
  
  # Removing qualitative data which has already been cleaned
  data$Seasons <- NULL 
  data$Holiday_Orig <- NULL
  data$Functioning_Day_Orig <- NULL
  return(data)
}


