install.packages(xlsx)
library(xlsx)
require(xlsx)

#replace blanks with NA's
All_Dates_Weather<- read.csv("C:/Users/saipr/Desktop/Sai Priya/KDD/SharkAttackDataSets/AttacksConsolidated_2011-2016.csv",
                             header=T, na.strings=c("","NA"))


All_Dates_Water<- read.csv("C:/Users/saipr/Desktop/Sai Priya/KDD/SharkAttackDataSets/water_data.csv",
                           header=T, na.strings=c("","NA"))

dim(All_Dates_Water)
dim(All_Dates_Weather)

#To merge water and weather
WeatherAndWater <- cbind(All_Dates_Weather,All_Dates_Water)
View(WeatherAndWater)
#To remove unwanted columns
WeatherAndWater$ï..DateTimeStamp <- NULL
WeatherAndWater$Attack <- NULL
WeatherAndWater$Date <- NULL
WeatherAndWater$Time <- NULL

#To format date and year
WeatherAndWater$DATE<-as.Date(WeatherAndWater$DATE, format('%m/%d/%Y'))
WeatherAndWater$year <- substring(WeatherAndWater$DATE,1,4)


View(WeatherAndWater)

