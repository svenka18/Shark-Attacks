

#z-score calculation
WeatherAndWater$Temperature_zscore <- (WeatherAndWater$DRYBULBTEMPF - mean(WeatherAndWater$DRYBULBTEMPF)) / sd(WeatherAndWater$DRYBULBTEMPF)
WeatherAndWater$WaterTemperature_zscore <- (WeatherAndWater$Temp - mean(WeatherAndWater$Temp)) / sd(WeatherAndWater$Temp)
WeatherAndWater$SeaLevelPressure_zscore <- (WeatherAndWater$SeaLevelPressure - mean(WeatherAndWater$SeaLevelPressure)) / sd(WeatherAndWater$SeaLevelPressure)
WeatherAndWater$WindSpeed_zscore <- (WeatherAndWater$WindSpeed - mean(WeatherAndWater$WindSpeed)) / sd(WeatherAndWater$WindSpeed)
WeatherAndWater$Salinity_zscore <- (WeatherAndWater$Sal-mean(WeatherAndWater$Sal))/sd(WeatherAndWater$Sal)
WeatherAndWater$Turbidity_zscore <- (WeatherAndWater$Turb-mean(WeatherAndWater$Turb))/sd(WeatherAndWater$Turb)

#min-max normalisation

WeatherAndWater$Temperature_minmax <- (WeatherAndWater$DRYBULBTEMPF - min(WeatherAndWater$DRYBULBTEMPF))/(min(WeatherAndWater$DRYBULBTEMPF) - max(WeatherAndWater$DRYBULBTEMPF))
WeatherAndWater$WaterTemperature_minmax <- (WeatherAndWater$Temp - min(WeatherAndWater$Temp))/(min(WeatherAndWater$Temp) - max(WeatherAndWater$Temp))
WeatherAndWater$SeaLevelPressure_minmax <- (WeatherAndWater$SeaLevelPressure - min(WeatherAndWater$SeaLevelPressure))/(min(WeatherAndWater$SeaLevelPressure) - max(WeatherAndWater$SeaLevelPressure))
WeatherAndWater$WindSpeed_minmax <- (WeatherAndWater$WindSpeed - min(WeatherAndWater$WindSpeed))/(min(WeatherAndWater$WindSpeed) - max(WeatherAndWater$WindSpeed))
WeatherAndWater$Salinity_minmax <- (WeatherAndWater$Sal-min(WeatherAndWater$Sal))/(max(WeatherAndWater$Sal)-min(WeatherAndWater$Sal))
WeatherAndWater$Turbidity_minmax <- (WeatherAndWater$Turb-min(WeatherAndWater$Turb))/(max(WeatherAndWater$Turb)-min(WeatherAndWater$Turb))
View(WeatherAndWater)

write.xlsx(WeatherAndWater,"C:/Users/saipr/Desktop/Sai Priya/KDD/SharkAttackDataSets/SharkAttack_UnBalanced.xlsx")
