library(lunar)
require(lunar)
library("date")
require("date")

WeatherAndWater$moonphase<-lunar.phase(as.Date(WeatherAndWater$DATE, format('%m/%d/%Y')),name=8)
WeatherAndWater$datePlusTwo <- as.Date(WeatherAndWater$DATE)+2
WeatherAndWater$dateMinusTwo <- as.Date(WeatherAndWater$DATE)-2
WeatherAndWater$moonphasePlusTwo <- lunar.phase(WeatherAndWater$datePlusTwo, name = 8)
WeatherAndWater$moonphaseMinusTwo <- lunar.phase(WeatherAndWater$dateMinusTwo, name = 8)

View(WeatherAndWater)

write.xlsx(WeatherAndWater,"C:/Users/saipr/Desktop/Sai Priya/KDD/SharkAttackDataSets/WeatherAndWater_BeforeCleaning.xlsx")

