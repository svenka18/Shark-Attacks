library(rpart)
require(rpart)
library(rpart.plot)
require(rpart.plot)
library(rattle)
require(rattle)
library(partykit)
require(partykit)
library(C50)
require(C50)
library(caret)
require(caret)
library(pROC)
require(pROC)

bal <- read.csv('C:/Users/saipr/Desktop/Sai Priya/KDD/SharkAttackDataSets/SharkAttack_Balanced.csv')
shark.testing <- read.csv('C:/Users/saipr/Desktop/Sai Priya/KDD/SharkAttackDataSets/SharkAttackTesting.csv')

typeof(bal$salinity_discretize)
bal$salinity_discretize <- as.character(bal$salinity_discretize)
bal$salinity_discretize[bal$salinity_discretize == "High(35-40)"] <- "L1"
bal$salinity_discretize[bal$salinity_discretize == "Low(30-35)"] <- "L2"
bal$salinity_discretize[bal$salinity_discretize == "L1"] <- "Low(20-30)"
bal$salinity_discretize[bal$salinity_discretize == "L2"] <- "High(35-40)"

View(shark.testing)
sapply(bal, function(x) sum(is.na(x))) 

bal$watertemp_discretize <- bal$DRYBULBTEMPF_discretize
shark.testing$watertemp_discretize <- shark.testing$DRYBULBTEMPF_discretize

#discretized variables
baltree1<- subset(bal, select = c(WindDirection_discretize, salinity_discretize, WindSpeed_discretize
                                  ,watertemp_discretize ,Attack..Y.N.))
DecisionTreeModel <- rpart(Attack..Y.N. ~ WindSpeed_discretize+watertemp_discretize+WindDirection_discretize+salinity_discretize, data = bal, method = "class",minsplit=2, minbucket=1)
c50 <- C5.0(Attack..Y.N. ~.,data = baltree1) 
summary(c50)
DecisionTreeModel1 <- ctree(Attack..Y.N. ~ ., data = baltree1)

rpart.plot(DecisionTreeModel)
plot(c50)
plot(DecisionTreeModel1)

#CM
#FP's are more for ctree
PredictionsWithClass <- predict(DecisionTreeModel,shark.testing,type="class")
t <- table(PredictionsWithClass,shark.testing$Attack..Y.N.)
cm <- confusionMatrix(PredictionsWithClass,shark.testing$Attack..Y.N.) #accuracy=0.76

#ROC
rpart2_probs<-(predict(DecisionTreeModel, type="prob",shark.testing))[,2]
plot(roc(shark.testing$Attack..Y.N.,rpart2_probs))
abline(a=0,b=1)
auc <- auc(shark.testing$Attack..Y.N.,rpart2_probs)


pr <- prediction(rpart2_probs, shark.testing$attack)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

#accuracy matrix
#the accuracy of our model is 84.7%
sum(diag(t/sum(t)))

#another tree model
#discretized variables
baltree3<- subset(bal, select = c(moonphase,
                                  SeaLevelPressure_minmax,WindSpeed_minmax,
                                  Turbidity_minmax,Attack..Y.N.))
DecisionTreeModel3 <- rpart(Attack..Y.N. ~ moonphaseMinusTwo+WaterTemperature_minmax
                            +Turbidity_minmax, data = bal, method = "class",minsplit=2, minbucket=1)
DecisionTreeModel33 <- ctree(Attack..Y.N. ~ ., data = baltree3)

rpart.plot(DecisionTreeModel3)
plot(DecisionTreeModel33)
#CM
PredictionsWithClass2 <- predict(DecisionTreeModel3,shark.testing,type="class")
CM2 <- confusionMatrix(PredictionsWithClass2,shark.testing$Attack..Y.N.) #accuracy=0.78

#ROC
rpart3_probs<-(predict(DecisionTreeModel3, type="prob",shark.testing))[,2]
plot(roc(shark.testing$Attack..Y.N.,rpart3_probs))
abline(a=0,b=1)
auc <- auc(shark.testing$Attack..Y.N.,rpart3_probs) #0.57


