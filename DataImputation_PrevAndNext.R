library(stringr)
require(stringr)
require(zoo)

#Identify invalid data
is.numeric(WeatherAndWater$DRYBULBTEMPF)
is.numeric(WeatherAndWater$WindSpeed)
is.numeric(WeatherAndWater$WindDirection)
is.numeric(WeatherAndWater$SeaLevelPressure)
is.numeric(WeatherAndWater$Sal)
is.numeric(WeatherAndWater$Turb)
typeof(WeatherAndWater$Attack..Y.N.)


#temperature

#To remove values that contains character. Replaces the entire value with blanks
WeatherAndWater$DRYBULBTEMPF <- str_sub(WeatherAndWater$DRYBULBTEMPF, 1, str_length(WeatherAndWater$DRYBULBTEMPF)-3)
#To replace the blanks with 0
WeatherAndWater$DRYBULBTEMPF <- sub("^$", 0 , WeatherAndWater$DRYBULBTEMPF)
WeatherAndWater$DRYBULBTEMPF<-as.numeric(WeatherAndWater$DRYBULBTEMPF)


#Wind direction
WeatherAndWater$WindDirection[WeatherAndWater$WindDirection=="VRB"]<-0
WeatherAndWater$WindDirection <- as.numeric(WeatherAndWater$WindDirection)

#identify NA's
sapply(WeatherAndWater, function(x) sum(is.na(x))) 

#Handling NA's 
#Temp
WeatherAndWater$DRYBULBTEMPF <- na.approx(WeatherAndWater$DRYBULBTEMPF)


#water Temp
WeatherAndWater$Temp <- round(na.approx(WeatherAndWater$Temp),digits = 1)

#Windspeed
WeatherAndWater$WindSpeed <- na.approx(WeatherAndWater$WindSpeed)

#Winddirection
WeatherAndWater$WindDirection <- na.approx(WeatherAndWater$WindDirection)
#Salinity
WeatherAndWater$Sal <- round(na.approx(WeatherAndWater$Sal), digits = 1)
#Turbidity
WeatherAndWater$Turb <- na.approx(WeatherAndWater$Turb)

sapply(WeatherAndWater, function(x) sum(is.na(x))) 


