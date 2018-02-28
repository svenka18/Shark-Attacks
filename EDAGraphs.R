

#install package to draw boxplot
install.packages("ggplot2")
library(ggplot2)
require(ggplot2)

#############################EDA

WeatherAndWater_Yes <- SharkAttack_Unbalanced[SharkAttack_Unbalanced$Attack..Y.N. == "Y",]
View(WeatherAndWater_Yes)

sapply(WeatherAndWater_Yes, function(x) sum(is.na(x))) 


#ggplot of sealevel pressure after binning
ggplot() + geom_bar(data =WeatherAndWater_Yes, 
                    aes(x=factor(WeatherAndWater_Yes$SeaLevelPressure_discretize)),
                    position="stack")+xlab("Sea Level Pressure")+ggtitle("Distribution of Pressure")


#ggplot of temperature after binning

ggplot() + geom_bar(data =WeatherAndWater_Yes, 
                    aes(x=factor(WeatherAndWater_Yes$DRYBULBTEMPF_discretize)),
                    position="stack")+xlab("Temperature")+ggtitle("Distribution of temperature")

ggplot() + geom_bar(data =WeatherAndWater_Yes, 
                    aes(x=factor(WeatherAndWater_Yes$temp_discretize)),
                    position="stack")+xlab("Temperature")+ggtitle("Distribution of temperature")


cut(WeatherAndWater_Yes$DRYBULBTEMPF,pretty(DRYBULBTEMPF,2))


ggplot() + geom_bar(data =WeatherAndWater_Yes, 
                    aes(x=factor(cut(WeatherAndWater_Yes$DRYBULBTEMPF,pretty(DRYBULBTEMPF,2)))),
                    position="stack")+xlab("Temperature")+ggtitle("Distribution of temperature")

ggplot() + geom_bar(data =WeatherAndWater_Yes, 
                    aes(x=factor(cut(WeatherAndWater_Yes$Temp,pretty(DRYBULBTEMPF,2)))),
                    position="stack")+xlab("Temperature")+ggtitle("Distribution of temperature")


#ggplot of windspeed after binning

ggplot() + geom_bar(data =WeatherAndWater_Yes, 
                    aes(x=factor(WeatherAndWater_Yes$WindSpeed_discretize)),
                    position="stack")+xlab("Wind Speed")+ggtitle("Distribution of Wind speed")



#ggplot of winddirection after binning

ggplot() + geom_bar(data =WeatherAndWater_Yes, 
                    aes(x=factor(WeatherAndWater_Yes$WindDirection_discretize)),
                    position="stack")+xlab("Wind Direction")+ggtitle("Distribution of Wind direction")


#ggplot of Salinity after binning

ggplot() + geom_bar(data =WeatherAndWater_Yes, 
                    aes(x=factor(WeatherAndWater_Yes$salinity_discretize)),
                    position="stack")+xlab("Salinity")+ggtitle("Distribution of Salinity")


#ggplot of turbidity after binning

ggplot() + geom_bar(data =WeatherAndWater_Yes, 
                    aes(x=factor(WeatherAndWater_Yes$Turbidity_discretize)),
                    position="stack")+xlab("Turbidity")+ggtitle("Distribution of Turbidity")

hist(WeatherAndWater_Yes$Turb)
#ggplot of Moon Phase

ggplot() + geom_bar(data =WeatherAndWater_Yes, 
                    aes(x=factor(WeatherAndWater_Yes$moonphase)),
                    position="stack")+xlab("MoonPhase")+ggtitle("Distribution of MoonPhase")
WeatherAndWater_Yes$moonphase[WeatherAndWater_Yes$moonphase == "Last quarter"] <- "L1"
WeatherAndWater_Yes$moonphase[WeatherAndWater_Yes$moonphase == "New"] <- "L2"
WeatherAndWater_Yes$moonphase[WeatherAndWater_Yes$moonphase == "L1"] <- "New"
WeatherAndWater_Yes$moonphase[WeatherAndWater_Yes$moonphase == "L2"] <- "Last quarter"

WeatherAndWater_Yes$Turbidity_discretize[WeatherAndWater_Yes$salinity_discretize == "High(35-40)"] <- "L1"
WeatherAndWater_Yes$Turbidity_discretize[WeatherAndWater_Yes$salinity_discretize == "Low(30-35)"] <- "L2"
WeatherAndWater_Yes$Turbidity_discretize[WeatherAndWater_Yes$salinity_discretize == "L1"] <- "Low(20-30)"
WeatherAndWater_Yes$salinity_discretize[WeatherAndWater_Yes$salinity_discretize == "L2"] <- "High(35-40)"
