install.packages("infotheo") 
library(infotheo)
require(infotheo)

####################################

#Sea Level Pressure

#binning sea level pressure
seapressure_dis <- discretize(WeatherAndWater$SeaLevelPressure,disc = "equalwidth",3)
WeatherAndWater$SeaLevelPressure_discretize <- seapressure_dis $X 
View(seapressure_dis)
summary(WeatherAndWater$SeaLevelPressure_discretize)
WeatherAndWater$SeaLevelPressure_discretize[WeatherAndWater$SeaLevelPressure_discretize == 1] = 'Low'
WeatherAndWater$SeaLevelPressure_discretize[WeatherAndWater$SeaLevelPressure_discretize == 2] = 'Medium'
WeatherAndWater$SeaLevelPressure_discretize[WeatherAndWater$SeaLevelPressure_discretize == 3] = 'High'
summary(WeatherAndWater$SeaLevelPressure_discretize)




#Temperature

temperature_dis <- discretize(WeatherAndWater$DRYBULBTEMPF,disc = "equalwidth",3)
WeatherAndWater$DRYBULBTEMPF_discretize <- temperature_dis $X 
WeatherAndWater$DRYBULBTEMPF_discretize
View(temperature_dis)
summary(WeatherAndWater$DRYBULBTEMPF_discretize)
WeatherAndWater$DRYBULBTEMPF_discretize[WeatherAndWater$DRYBULBTEMPF_discretize == 1] = 'Low'
WeatherAndWater$DRYBULBTEMPF_discretize[WeatherAndWater$DRYBULBTEMPF_discretize == 2] = 'Medium(50-65)'
WeatherAndWater$DRYBULBTEMPF_discretize[WeatherAndWater$DRYBULBTEMPF_discretize == 3] = 'High(65-95)'
summary(WeatherAndWater$DRYBULBTEMPF_discretize)


#Water Temperature

temp_dis <- discretize(WeatherAndWater$Temp,disc = "equalwidth",3)
WeatherAndWater$temp_discretize <- temp_dis $X 
WeatherAndWater$temp_discretize[WeatherAndWater$temp_discretize == 1] = 'Low(0-15)'
WeatherAndWater$temp_discretize[WeatherAndWater$temp_discretize == 2] = 'Medium(16-20)'
WeatherAndWater$temp_discretize[WeatherAndWater$temp_discretize == 3] = 'High(>20)'





#WInd Speed

#binning wind speed
WeatherAndWater$WindSpeed_discretize[WeatherAndWater$WindSpeed >= 10] <- "High(>10)"
WeatherAndWater$WindSpeed_discretize[WeatherAndWater$WindSpeed >= 5 & WeatherAndWater$WindSpeed < 10] <- "Medium(5-10)"
WeatherAndWater$WindSpeed_discretize[WeatherAndWater$WindSpeed > 0 & WeatherAndWater$WindSpeed < 5] <- "Low(0-5)"
WeatherAndWater$WindSpeed_discretize[WeatherAndWater$WindSpeed == 0] <- "None(0)"

#WIndDirection

#binning wind direction
WeatherAndWater$WindDirection_discretize[WeatherAndWater$WindDirection >= 340 | WeatherAndWater$WindDirection < 10] <- "North"
WeatherAndWater$WindDirection_discretize[WeatherAndWater$WindDirection >= 10 & WeatherAndWater$WindDirection < 80] <- "Northeast"
WeatherAndWater$WindDirection_discretize[WeatherAndWater$WindDirection >= 80 & WeatherAndWater$WindDirection < 100] <- "East"
WeatherAndWater$WindDirection_discretize[WeatherAndWater$WindDirection >= 100 & WeatherAndWater$WindDirection < 170] <- "Southeast"
WeatherAndWater$WindDirection_discretize[WeatherAndWater$WindDirection >= 170 & WeatherAndWater$WindDirection < 190] <- "South"
WeatherAndWater$WindDirection_discretize[WeatherAndWater$WindDirection >= 190 & WeatherAndWater$WindDirection < 260] <- "Southwest"
WeatherAndWater$WindDirection_discretize[WeatherAndWater$WindDirection >= 260 & WeatherAndWater$WindDirection < 280] <- "West"
WeatherAndWater$WindDirection_discretize[WeatherAndWater$WindDirection >= 280 & WeatherAndWater$WindDirection < 350] <- "Northwest"
WeatherAndWater$WindDirection_discretize[WeatherAndWater$WindSpeed==0] <- "None"



#Salinity

salinity_dis <- discretize(WeatherAndWater$Sal,disc = "equalwidth",2)
WeatherAndWater$salinity_discretize <- salinity_dis $X 
WeatherAndWater$salinity_discretize[WeatherAndWater$salinity_discretize == 1] = 'Low(30-35)'
WeatherAndWater$salinity_discretize[WeatherAndWater$salinity_discretize == 2] = 'High(35-40)'


#Turbidity

turbidity_dis <- discretize(WeatherAndWater$Turb,disc = "equalwidth",3)
WeatherAndWater$Turbidity_discretize <- turbidity_dis $X 
WeatherAndWater$Turbidity_discretize[WeatherAndWater$Turbidity_discretize == 1] = 'Low'
WeatherAndWater$Turbidity_discretize[WeatherAndWater$Turbidity_discretize == 2] = 'Medium'
WeatherAndWater$Turbidity_discretize[WeatherAndWater$Turbidity_discretize == 3] = 'High'

View(WeatherAndWater)




