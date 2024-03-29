library(ggplot2)
library(forcats)
library(dplyr)

setwd("~/GitHub/IE-490-Group-7")
PlotData <- read.csv('SeoulBikeData.csv', encoding = "latin1")

# Plots for quantitative variables vs Rented Bikes Count
plot(PlotData$Hour, PlotData$Rented_Bikes, main = "Hour vs Rented Bikes Count", 
     xlab = "Hour", ylab = "Rented Bikes Count")
plot(PlotData$Temp, PlotData$Rented_Bikes, main = "Temperature vs Rented Bikes Count", 
     xlab = "Temperature(°C)", ylab = "Rented Bikes Count")
plot(PlotData$Humidity, PlotData$Rented_Bikes, main = "Humidity vs Rented Bikes Count", 
     xlab = "Humidity(%)", ylab = "Rented Bikes Count")
plot(PlotData$Wind_speed, PlotData$Rented_Bikes, main = "Wind Speed vs Rented Bikes Count", 
     xlab = "Wind Speed (m/s)", ylab = "Rented Bikes Count")
plot(PlotData$Visibility, PlotData$Rented_Bikes, main = "Visibility vs Rented Bikes Count", 
     xlab = "Visibility (10m)", ylab = "Rented Bikes Count")
plot(PlotData$Dew_Point_Temp, PlotData$Rented_Bikes, main = "Dew Point Temperature vs Rented Bikes Count", 
     xlab = "Dew Point Temperature (°C)", ylab = "Rented Bikes Count")
plot(PlotData$Solar_Radiation, PlotData$Rented_Bikes, main = "Solar Radiation vs Rented Bikes Count", 
     xlab = "Solar Radiation (MJ/m2)", ylab = "Rented Bikes Count")
plot(PlotData$Rainfall, PlotData$Rented_Bikes, main = "Rainfall vs Rented Bikes Count", 
     xlab = "Rainfall(mm)", ylab = "Rented Bikes Count")
plot(PlotData$Snowfall, PlotData$Rented_Bikes, main = "Snowfall vs Rented Bikes Count", 
     xlab = "Snowfall(cm)", ylab = "Rented Bikes Count")

# Violin Plot for Rented Bikes Count by Season
PlotData %>% 
  mutate(Seasons = fct_relevel(Seasons, "Winter", "Spring", "Summer", "Autumn")) %>% 
  ggplot( aes(x = Seasons, y = Rented_Bikes, fill=Seasons)) + geom_violin()
