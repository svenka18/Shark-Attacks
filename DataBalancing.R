library(ROSE)
require(ROSE)
require(xlsx)
require(caret)

SharkAttack_Unbalanced <- read.csv(file="C:/Users/saipr/Desktop/Sai Priya/KDD/SharkAttackDataSets/SharkAttack_UnBalanced.csv", stringsAsFactors = FALSE)
View(SharkAttack_Unbalanced)
SharkAttack_Unbalanced$part <- NULL

#partitioning into training and testing datasets
#SharkAttack_Unbalanced$part <- runif(length(SharkAttack_Unbalanced$Attack..Y.N.), min=0, max=1) 
#shark.training <- SharkAttack_Unbalanced[SharkAttack_Unbalanced$part <= 0.75,]
#shark.testing <- SharkAttack_Unbalanced[SharkAttack_Unbalanced$part > 0.75,] 
#dim(shark.training);dim(shark.testing) 


#another way of partitioning

trainIndex <- createDataPartition(SharkAttack_Unbalanced$Attack..Y.N., p = 0.75, 
                                  list = FALSE, 
                                  times = 1)

shark.training = SharkAttack_Unbalanced[trainIndex,]
shark.testing = SharkAttack_Unbalanced[-trainIndex,]
dim(shark.training);dim(shark.testing)

imputation <- preProcess(shark.training, method=c("center", "scale"))

shark.training <- predict(imputation, training)
shark.testing <- predict(imputation, test)
View(shark.training)

#Balancing

shark.training$attack <- 
  as.character( 
    factor( 
      shark.training$Attack..Y.N., 
      levels = c("N", "Y"), 
      labels = c("0", "1")))

shark.testing$attack <- 
  as.character( 
    factor( 
      shark.testing$Attack..Y.N., 
      levels = c("N", "Y"), 
      labels = c("0", "1")))


#shark.balanced <- ROSE(attack~., shark.training, N = 1780)
#shark.balanced.ou <- ovun.sample(attack~., data=shark.training,
#                                 N= 1780, p=0.5, 
#                               seed=1, method="both")$data

shark.balanced.ou <- ovun.sample(attack~., data=shark.training,
                                 p=0.4, 
                                 seed=1, method="over")$data

shark.balanced.ou.test <- ovun.sample(attack~., data=shark.testing,
                                      p=0.4, 
                                      seed=1, method="over")$data

t <- table(shark.balanced.ou$Attack..Y.N.)
table(shark.balanced.ou.test$attack)
t1 <- table(shark.training$Attack..Y.N.)
table(shark.testing$attack)
lbls <- c("N","Y")

par(mfrow=c(1,1))
pie(t1,label=c("Attack = N","Attack= Y"),col=c("red","blue"),main="Before Balancing")
pie(t,label=c("Attack = N","Attack= Y"),col=c("red","blue"),main="After Balancing")

View(shark.balanced.ou)

write.xlsx(shark.balanced.ou,"C:/Users/saipr/Desktop/Sai Priya/KDD/SharkAttackDataSets/SharkAttack_Balanced.xlsx")
write.xlsx(training,"C:/Users/saipr/Desktop/Sai Priya/KDD/SharkAttackDataSets/SharkAttackTraining.xlsx")
write.xlsx(test,"C:/Users/saipr/Desktop/Sai Priya/KDD/SharkAttackDataSets/SharkAttackTesting.xlsx")


#############################

#Using SMOTE

table(WeatherAndWater$attack)
newData <- SMOTE(attack ~ ., WeatherAndWater, perc.over = 0,perc.under=0)
table(newData$attack)

warnings()



