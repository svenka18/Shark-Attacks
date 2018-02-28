
#replacing blanks with NA
global_shark_attack <- read.csv("C:/Users/saipr/Desktop/Sai Priya/KDD/shar Attack/Shark Attack May-Oct.csv", header=T, na.strings=c("","NA"))
View(global_shark_attack)



par(mfrow=c(1,2))               # set graphics window to plot side by side

#years on which shark attack happened
table(global_shark_attack$Year)
hist(global_shark_attack$Year,main="Year of Attacks",xlab="Year")

#no of fatal attacks
table(shark_data$Fatal..Y.N.)
ggplot() + geom_bar(data =global_shark_attack, 
                    aes(x=factor(global_shark_attack$Fatal..Y.N.)),
                    position="stack")+xlab("Fatality")+ggtitle("Fatality of Attacks")


#Attacks in each county
table(shark_data$County)
#normalised overlayed barchart for county vs attacks
ggplot() + geom_bar(data =global_shark_attack, 
                    aes(x=factor(global_shark_attack$County)),
                    position="stack")+coord_flip()+xlab("County")+ggtitle("Attacks by County")

#Activity
ggplot() + geom_bar(data =global_shark_attack, 
                    aes(x=factor(global_shark_attack$Activity)),
                    position="stack")+coord_flip()+xlab("Activity")+ggtitle("Attacks by activity")

#Type
ggplot() + geom_bar(data =global_shark_attack, 
                    aes(x=factor(global_shark_attack$Type)),
                    position="stack")+xlab("Type")+ggtitle("Type Of Attack")






