library(ROCR)
require(ROCR)
typeof(shark.balanced.ou$attack)
shark.balanced.ou$attack <- as.factor(bal$attack)
bal$WindDirection_discretize <- as.factor(bal$attack)

#Logistic Regression
glm.fit<-glm(attack ~ WindSpeed	+ WindDirection_discretize +	SeaLevelPressure+
               + Temp	+ Sal +	Turb + moonphase + moonphasePlusTwo	+ moonphaseMinusTwo,
             family=binomial, data = bal)

summary(glm.fit)
logLik(glm.fit)

glm_probs <- predict(glm.fit, subset(shark.testing, select = c(WindSpeed, WindDirection_discretize, SeaLevelPressure
                                                               ,Temp ,Sal,Turb,moonphase,moonphasePlusTwo,moonphaseMinusTwo,attack)),type="response")

glm_pred <- ifelse(glm_probs>0.5, "Attack", "No Attack")
#roc
pr <- prediction(glm_probs, shark.testing$attack)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
AIC(glm.fit, k = 2)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]

#Other classifers 
library(MASS)
lda.fit<-lda(Attack..Y.N. ~ DRYBULBTEMPF + WindSpeed	+ WindDirection +	SeaLevelPressure
             + Temp	+ Sal +	Turb + moonphase + moonphasePlusTwo	+ moonphaseMinusTwo
             , data = bal)
lda_pred <- predict(lda.fit, test)
lda_probs <- lda_pred$posterior[,2]
confusionMatrix(table(lda_pred$class, test$Attack..Y.N.))
plotROC(lda_probs, add=TRUE, col="red")
calculateAUC(lda_probs)


library(e1071)
bayes.fit<-naiveBayes(Attack..Y.N. ~ DRYBULBTEMPF + WindSpeed	+ WindDirection +	SeaLevelPressure
                      + Temp	+ Sal +	Turb + moonphase + moonphasePlusTwo	+ moonphaseMinusTwo
                      , data = bal)
bayes_pred<-predict(bayes.fit,test)
bayes_probs<-(predict(bayes.fit, type="raw", test))[,2]
confusionMatrix(table(bayes_pred, test$Attack..Y.N.))
plotROC(bayes_probs, add=TRUE, col="cyan")
calculateAUC(bayes_probs)






