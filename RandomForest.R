library(party)
require(party)
library(randomForest)
require(randomForest)
require(ROCR)

bal <- read.csv('C:/Users/saipr/Desktop/Sai Priya/KDD/SharkAttackDataSets/SharkAttack_Balanced.csv')
str(bal)
bal$attack <- as.factor(bal$attack)
shark.testing$attack <- as.factor(shark.testing$attack)

####discretise and normal variables
rf_data <- subset(bal, select = c(WindSpeed, WindDirection_discretize, SeaLevelPressure
                                  ,DRYBULBTEMPF_discretize ,salinity_discretize,Turb,moonphase,moonphasePlusTwo,moonphaseMinusTwo,attack))

rf <- randomForest(attack~.,data=rf_data,ntree=100)
print(rf)
str(rf_data)
mtry <- tuneRF(rf_data[-10],rf_data$attack,ntreeTry=100,stepFactor=1.5,improve=0.01,trace=TRUE,plot=TRUE)
best.m <- mtry[mtry[,2]==min(mtry[,2]),1]
print(mtry)
print(best.m)
rf <- randomForest(attack~.,data=rf_data,mtry=best.m,importance=TRUE,ntree=100)
print(rf)
importance(rf)
varImpPlot(rf)


#####normal variables

rf_data1 <- subset(bal, select = c(WindSpeed, WindDirection, SeaLevelPressure
                                   ,DRYBULBTEMPF ,Sal,Turb,moonphase,moonphasePlusTwo,moonphaseMinusTwo,attack))
test <-subset(shark.testing, select = c(WindSpeed, WindDirection, SeaLevelPressure
                                        ,DRYBULBTEMPF ,Sal,Turb,moonphase,moonphasePlusTwo,moonphaseMinusTwo,attack)) 

test$moonphase<-as.factor(test$moonphase)
test$moonphasePlusTwo<-as.factor(test$moonphasePlusTwo)
test$moonphaseMinusTwo<-as.factor(test$moonphaseMinusTwo)
test$attack<-as.factor(test$attack)

str(test)
str(rf_data1)

#random forest
rf1 <- randomForest(attack~.,data=rf_data1,ntree=100)
print(rf1)
mtry1 <- tuneRF(rf_data1[-10],rf_data1$attack,ntreeTry=100,stepFactor=1.5,improve=0.01,trace=TRUE,plot=TRUE)
best.m1 <- mtry1[mtry1[,2]==min(mtry1[,2]),1]
print(mtry1)
print(best.m1)
rf1 <- randomForest(attack~.,data=rf_data1,mtry=best.m1,importance=TRUE,ntree=100)
print(rf1)
importance(rf1)
varImpPlot(rf1)

#predictions on test data
prob_training=predict(rf1,type = "prob")
prob_testing=predict(rf1,type = "prob",test)
rf_prob <- ifelse(prob_testing>0.5, "Attack", "No Attack")
test$probability <- prob_testing
test$prediction<-rf_prob
View(test)
write.csv(test,"C:/Users/saipr/Desktop/Sai Priya/KDD/SharkAttackDataSets/RandomForestPredictions.csv")

#Model Performance
perf = prediction(prob_testing[,2], test$attack)
prf <- performance(perf, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(perf, measure = "auc")
auc <- auc@y.values[[1]]
#sample tree
cf<-cforest(attack~., data=rf_data1, controls=cforest_control(mtry=4, mincriterion=0))
pt <- prettytree(cf@ensemble[[1]], names(cf@data@get("input"))) 
nt <- new("BinaryTree") 
nt@tree <- pt 
nt@data <- cf@data 
nt@responses <- cf@responses 
plot(nt, type="simple")



#############min-max and discretised variables

rf_data2 <- subset(bal, select = c(WindSpeed_minmax, WindDirection_discretize, SeaLevelPressure_minmax
                                   ,DRYBULBTEMPF_discretize ,salinity_discretize,Turbidity_minmax,moonphase,moonphasePlusTwo,moonphaseMinusTwo,attack))


rf2 <- randomForest(attack~.,data=rf_data2,ntree=100)
print(rf2)
str(rf_data2)
mtry2 <- tuneRF(rf_data2[-10],rf_data2$attack,ntreeTry=100,stepFactor=1.5,improve=0.01,trace=TRUE,plot=TRUE)
best.m2 <- mtry2[mtry2[,2]==min(mtry2[,2]),1]
print(mtry2)
print(best.m2)
rf2 <- randomForest(attack~.,data=rf_data2,mtry=best.m2,importance=TRUE,ntree=100)
print(rf2)
importance(rf2)
varImpPlot(rf2)

####################min-max variables

rf_data3 <- subset(bal, select = c(WindSpeed_minmax, WindDirection_discretize, SeaLevelPressure_minmax
                                   ,Temperature_minmax ,Salinity_minmax,Turbidity_minmax,moonphase,moonphasePlusTwo,moonphaseMinusTwo,attack))

test <- subset(shark.testing, select = c(WindSpeed_minmax, WindDirection_discretize, SeaLevelPressure_minmax
                                         ,Temperature_minmax ,Salinity_minmax,Turbidity_minmax,moonphase,moonphasePlusTwo,moonphaseMinusTwo,attack))

str(rf_data3)
str(test)
test$WindDirection_discretize<-as.factor(test$WindDirection_discretize)
test$moonphase<-as.factor(test$moonphase)
test$moonphasePlusTwo<-as.factor(test$moonphasePlusTwo)
test$moonphaseMinusTwo<-as.factor(test$moonphaseMinusTwo)
test$attack<-as.factor(test$attack)
rf3 <- randomForest(attack~.,data=rf_data3,ntree=100)
print(rf3)
mtry3 <- tuneRF(rf_data3[-10],rf_data3$attack,ntreeTry=100,stepFactor=1.5,improve=0.01,trace=TRUE,plot=TRUE)
best.m3 <- mtry3[mtry3[,2]==min(mtry3[,2]),1]
print(mtry3)
print(best.m3)
rf3 <- randomForest(attack~.,data=rf_data3,mtry=best.m3,importance=TRUE,ntree=100)
print(rf3)
importance(rf3)
varImpPlot(rf3)


#predictions on test data
prob_training=predict(rf3,type = "prob")
prob_testing=predict(rf3,type = "prob",test)
rf_prob <- ifelse(prob_testing>0.5, "Attack", "No Attack")
test$probability <- prob_testing
test$prediction<-rf_prob
View(test)
write.csv(test,"C:/Users/saipr/Desktop/Sai Priya/KDD/SharkAttackDataSets/RandomForestPredictions.csv")

#Model Performance
perf = prediction(prob_testing[,2], test$attack)
prf <- performance(perf, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(perf, measure = "auc")
auc <- auc@y.values[[1]]
#sample tree
cf<-cforest(attack~., data=rf_data3, controls=cforest_control(mtry=4, mincriterion=0))
tr <- party:::prettytree(cf@ensemble[[1]], names(cf@data@get("input"))) 
#plot(new("BinaryTree", tree=cf, data=cf@data, responses=cf@responses))

#nt <- new("BinaryTree") 
#nt@tree <- pt 
#nt@data <- cf@data 
#nt@responses <- cf@responses 
#plot(nt, type="simple")
ct <- ctree(attack ~ ., data=rf_data1)


update_tree <- function(ct) {
  if(!ct$terminal) {
    ct$left <- update_tree(ct$left)
    ct$right <- update_tree(ct$right)
  } else {
    ct$weights <- ct[[9]]
    ct$weights_ <- ct[[9]]
  }
  ct
}
tr_weights <- update_tree(tr)
plot(new("BinaryTree", tree=tr_weights, data=cf@data, responses=cf@responses))

###Results
importance(rf)
importance(rf1)
importance(rf2)
importance(rf3)

par(mfrow=c(4,1))

varImpPlot(rf) 
varImpPlot(rf1)
varImpPlot(rf2)
varImpPlot(rf3)


