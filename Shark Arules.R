library(arules)
require(arules)

bal <- read.csv('C:/Users/saipr/Desktop/Sai Priya/KDD/SharkAttackDataSets/SharkAttack_Balanced.csv')

shark.association1<- subset(bal, select = c(WindDirection_discretize, salinity_discretize, WindSpeed_discretize,	SeaLevelPressure_discretize
                                            ,temp_discretize ,moonphase,Attack..Y.N.))

shark.rules1 <- apriori(shark.association1, 
                        parameter=list(minlen=2, supp=0.005, conf=0.75), 
                        appearance = list(rhs=c("Attack..Y.N.=Y", "Attack..Y.N.=N"), 
                                          default="lhs"))

rules.sub1 <- subset(shark.rules1, subset = rhs %pin% "Attack..Y.N.=Y")
inspect(sort(rules.sub1, by="lift"))

############moonphase

shark.association2<- subset(bal, select = c(WindDirection_discretize, WindSpeed_discretize
                                            ,temp_discretize ,moonphase,Attack..Y.N.))

shark.rules2 <- apriori(shark.association2, 
                        parameter=list(minlen=2, supp=0.005, conf=0.75), 
                        appearance = list(rhs=c("Attack..Y.N.=Y", "Attack..Y.N.=N"), 
                                          default="lhs"))

rules.sub2 <- subset(shark.rules2, subset = rhs %pin% "Attack..Y.N.=Y")
inspect(sort(rules.sub2, by="lift"))

############moonphasePlusTwo

shark.association3<- subset(bal, select = c(WindDirection_discretize, WindSpeed_discretize
                                            ,temp_discretize ,moonphasePlusTwo,Attack..Y.N.))

shark.rules3 <- apriori(shark.association3, 
                        parameter=list(minlen=2, supp=0.005, conf=0.75), 
                        appearance = list(rhs=c("Attack..Y.N.=Y", "Attack..Y.N.=N"), 
                                          default="lhs"))

rules.sub3 <- subset(shark.rules3, subset = rhs %pin% "Attack..Y.N.=Y")
inspect(sort(rules.sub3, by="lift"))

############

shark.association4<- subset(bal, select = c(WindDirection_discretize,moonphasePlusTwo,WindSpeed_discretize
                                            ,Attack..Y.N.))

shark.rules4 <- apriori(shark.association4, 
                        parameter=list(minlen=2, supp=0.005, conf=0.75), 
                        appearance = list(rhs=c("Attack..Y.N.=Y", "Attack..Y.N.=N"), 
                                          default="lhs"))

rules.sub4 <- subset(shark.rules4, subset = rhs %pin% "Attack..Y.N.=Y")
inspect(sort(rules.sub4, by="lift"))

############No results

shark.association5<- subset(bal, select = c(WindDirection_discretize,temp_discretize
                                            ,Attack..Y.N.))

shark.rules5 <- apriori(shark.association5, 
                        parameter=list(minlen=2, supp=0.005, conf=0.75), 
                        appearance = list(rhs=c("Attack..Y.N.=Y", "Attack..Y.N.=N"), 
                                          default="lhs"))

rules.sub5 <- subset(shark.rules5, subset = rhs %pishark.rules5n% "Attack..Y.N.=Y")
inspect(sort(rules.sub5, by="lift"))

############no results

shark.association6<- subset(bal, select = c(WindDirection_discretize
                                            ,moonphasePlusTwo,Attack..Y.N.))

shark.rules6 <- apriori(shark.association6, 
                        parameter=list(minlen=2, supp=0.005, conf=0.75), 
                        appearance = list(rhs=c("Attack..Y.N.=Y", "Attack..Y.N.=N"), 
                                          default="lhs"))

rules.sub6 <- subset(shark.rules6, subset = rhs %pin% "Attack..Y.N.=Y")
inspect(sort(rules.sub6, by="lift"))

############ no results

shark.association7<- subset(bal, select = c(WindSpeed_discretize
                                            ,moonphasePlusTwo,Attack..Y.N.))

shark.rules7 <- apriori(shark.association7, 
                        parameter=list(minlen=2, supp=0.005, conf=0.75), 
                        appearance = list(rhs=c("Attack..Y.N.=Y", "Attack..Y.N.=N"), 
                                          default="lhs"))

rules.sub7 <- subset(shark.rules7, subset = rhs %pin% "Attack..Y.N.=Y")
inspect(sort(rules.sub7, by="lift"))

############moonphasePlusTwo  WindDirection_discretize, WindSpeed_discretize

shark.association8<- subset(bal, select = c(salinity_discretize,temp_discretize,moonphasePlusTwo
                                            ,Attack..Y.N.))

shark.rules8 <- apriori(shark.association8, 
                        parameter=list(minlen=2, supp=0.005, conf=0.75), 
                        appearance = list(rhs=c("Attack..Y.N.=Y", "Attack..Y.N.=N"), 
                                          default="lhs"))

rules.sub8 <- subset(shark.rules8, subset = rhs %pin% "Attack..Y.N.=Y")
inspect(sort(rules.sub8, by="lift"))

############moonphasePlusTwo  WindDirection_discretize, WindSpeed_discretize

shark.association9<- subset(bal, select = c(WindSpeed_discretize,temp_discretize,moonphasePlusTwo
                                            ,Attack..Y.N.))

shark.rules9 <- apriori(shark.association9, 
                        parameter=list(minlen=2, supp=0.005, conf=0.75), 
                        appearance = list(rhs=c("Attack..Y.N.=Y", "Attack..Y.N.=N"), 
                                          default="lhs"))

rules.sub9 <- subset(shark.rules9, subset = rhs %pin% "Attack..Y.N.=Y")
inspect(sort(rules.sub9, by="lift"))

############moonphasePlusTwo  WindDirection_discretize, WindSpeed_discretize

shark.association10<- subset(bal, select = c(WindDirection_discretize,temp_discretize,moonphaseMinusTwo
                                             ,Attack..Y.N.))

shark.rules10 <- apriori(shark.association10, 
                         parameter=list(minlen=2, supp=0.005, conf=0.75), 
                         appearance = list(rhs=c("Attack..Y.N.=Y", "Attack..Y.N.=N"), 
                                           default="lhs"))

rules.sub10 <- subset(shark.rules10, subset = rhs %pin% "Attack..Y.N.=Y")
inspect(sort(rules.sub10, by="lift"))

############moonphasePlusTwo  WindDirection_discretize, WindSpeed_discretize

shark.association11<- subset(bal, select = c(salinity_discretize, moonphasePlusTwo
                                             ,Attack..Y.N.))

shark.rules11 <- apriori(shark.association11, 
                         parameter=list(minlen=2, supp=0.005, conf=0.75), 
                         appearance = list(rhs=c("Attack..Y.N.=Y", "Attack..Y.N.=N"), 
                                           default="lhs"))

rules.sub11 <- subset(shark.rules11, subset = rhs %pin% "Attack..Y.N.=Y")
inspect(sort(rules.sub11, by="lift"))

