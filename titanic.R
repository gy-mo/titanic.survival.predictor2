getwd()
setwd("C:/Users/ram/Desktop/R/titanic")
train <- read.csv("train.csv",header = TRUE)
test <- read.csv("test.csv", header = TRUE)
str(train)
train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)
#add a survive var
test.survived <- data.frame(Survived = rep("none",nrow(test)),test
                            [,])
#combine data sets 
data.combined <- rbind(train,test.survived)
str(data.combined)
table(data.combined$Survived)
plot(data.combined$Survived)
table(data.combined$Pclass)

plot(data.combined$Pclass)
plot(data.combined$Survived,data.combined$Pclass)

#ggplot
library(ggplot2)
ggplot(train,aes(x=Sex,fill=factor(Survived)))+
  geom_bar(width=0.5)+
  xlab("class") +
  ylab("survived")
  
head(as.character(train$Name))
str(data.combined)


#pull out unique characters
length(unique(as.character(data.combined$Name)))

#pull our duplicate names
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])

library(stringr)
#any correlation with any other variable

misses <- data.combined[which(str_detect(data.combined$Name,"Miss.")),]
misses[1:5,]

mrses <- data.combined[which(str_detect(data.combined$Name , "Mrs.")),]
mrses[1:5,]

males <- data.combined[which(train$Sex == "male"),]


#define function
extractTitle <- function(Name) {
  Name <- as.character(Name)
  
  if(length(grep("Miss.",Name)) > 0) {
    return("Miss.")
  } else if(length(grep("Master",Name)) > 0) {
    return("Master")
  } else if(length(grep("Mrs.",Name)) > 0) {
    return("Mrs.")
  }  else if(length(grep("Mr.",Name)) > 0) {
    return("Mr.")
  } else {
  return("other")
}
}

titles <- NULL
for(i in 1:nrow(data.combined)) {
  titles <- c(titles,extractTitle(data.combined[ i,"Name"]))
} 
View(titles)
data.combined$Title <- as.factor(titles)

ggplot(data.combined[1:891,],aes(x=Title[1:891],fill=Survived[1:891])) +
  geom_bar(width = 0.5) +
  facet_wrap(data.combined$Pclass[1:891]) +
  ggtitle("based on class,title") +
  xlab("Title") +
  ylab("number of people") +
  labs(fill="Survived")


ggplot(train,aes(x=Age,fill=Survived))+
  geom_histogram(binwidth = 10)+
  facet_wrap(~Sex + Pclass)+
  xlab("sex")+
  ylab("number of people")

#validate that master is a good proxy for age
boys <- data.combined[which(data.combined$Title == "Master"),]
summary(boys$Age)   

men <- data.combined[which(data.combined$Title == "Mr."),]
head(men)
summary(men$Age)

misses <- data.combined[which(data.combined$Title=="Miss."),]
summary(girls$Age)

miss.alone <- misses[which(misses$SibSp<=0 & misses$Parch<= 0),]
summary(miss.alone)

length(unique(data.combined$SibSp))
data.combined$SibSp <- as.factor(data.combined$SibSp)


ggplot(data.combined[1:891,],aes(x=SibSp,fill=Survived))+
  geom_bar(width = 1)+
  facet_wrap(~Pclass + Title)+
  ggtitle("pclass+title")+
  xlab("sibsp")+
  ylab("people")+
  labs(fill="survived")  

data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,],aes(x=Parch,fill=Survived))+
  geom_bar(width=10)+
  facet_wrap(~Pclass+Title)+
  ggtitle("PARCH-by class and age ")+
  xlab("parch")+
  ylab("no of people")
warnings()


temp.sibsp <- c(train$SibSp,test$SibSp)
temp.parch <- c(train$Parch,test$Parch)

data.combined$family.size <- as.factor(temp.sibsp+temp.parch+1)

ggplot(data.combined[1:891,],aes(x=family.size,fill=Survived))+
  geom_bar(width = 1)+
  facet_wrap(~Pclass + Title)+
  ggtitle("plot on family size")+
  xlab("hhf")+
  ylab("people")+
  ylim(0,300)+
  labs(fill = "Survived")


data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,],aes(x=Parch,fill=Survived))+
  geom_bar(width = 1)+
  facet_wrap(~Pclass + Title)+
  ggtitle("Survivabikity on parch")+
  xlab("parch")+
  ylab("num of people")+
  labs(fill = "Survived")



str(data.combined$Ticket)
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

ticket.first.char <- ifelse(data.combined$Ticket==" ","U",substr(data.combined$Ticket,1,1))

ticket.first.char[1:100]
View(ticket.first.char)
as.factor(ticket.first.char)
data.combined$ticket.first.char <- ticket.first.char
ggplot(data.combined,aes(x=ticket.first.char,fill=Survived))+
  geom_bar(width = 1)+
  facet_wrap(~Pclass + Title)+
  ggtitle("based on ticket")+
  xlab("ticket")+
  ylab("num of people")+
  labs(fill="Survived")



str(data.combined$Fare)
summary(data.combined$Fare)
ggplot(data.combined,aes(x= data.combined$Fare))+
  geom_bar(width = 10)

ggplot(data.combined[1:891,],aes(x=Fare,fill=Survived))+
  geom_bar(width = 10)+
  facet_wrap(~Pclass + Title)+
  ggtitle("by Fare")+
  xlab("xxx")+
  ylim(0,50)+
  ylab("yyy")+
  labs(fill = "Survived")


str(data.combined$Cabin)
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:20]
data.combined[which(data.combined$Cabin ==""),"Cabin"] <- "U"
data.combined$Cabin[1:200]
data.combined$Cabin.first.char <-as.factor(substr(data.combined$Cabin,1,1))
data.combined$Cabin <- c(as.character(train$Cabin),as.character(test$Cabin))
data.combined$Cabin <- as.character(data.combined$Cabin)
str(data.combined$Cabin)
str(data.combined$Cabin)
levels(data.combined$Cabin)
ggplot(data.combined[1:891,],aes(x=Cabin.first.char,fill=Survived))+
  geom_bar(width = 5)+
  facet_wrap(~Pclass+Title)+
  ylim(0,100)


data.combined$multiple.cabins <- as.factor(ifelse(str_detect(data.combined$Cabin," "),"Y","N"))

ggplot(data.combined[1:891,], aes(x = multiple.cabins, fill = Survived)) +
  geom_bar(width = 5) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.multiple") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")




# Does survivability depend on where you got onboard the Titanic?
str(data.combined$Embarked)
levels(data.combined$Embarked)


# Plot data for analysis
ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("embarked") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

##########ML MODEL- RANDOM FOREST ##################################################
#####################################################################
library(randomForest)
rf.train.1 <- data.combined[1:891,c("Pclass","Title")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x=rf.train.1,y=rf.label,importance = TRUE,ntree = 1000)
rf.1
varImpPlot(rf.1)


rf.train.2 <- data.combined[1:891,c("Pclass","Title","SibSp")]
set.seed(1234)
rf.2 <- randomForest(x=rf.train.2,y=rf.label,importance = TRUE,ntree = 1000)
rf.2
varImpPlot(rf.2)


rf.train.3 <- data.combined[1:891,c("Pclass","Title","Parch")]
set.seed(1234)
rf.3 <- randomForest(x=rf.train.3,y=rf.label,importance = TRUE,ntree = 1000)
rf.3
varImpPlot(rf.3)


rf.train.4 <- data.combined[1:891,c("Pclass","Title","SibSp","Parch")]
set.seed(1234)
rf.4 <-randomForest(x=rf.train.4,y=rf.label,importance = TRUE,ntree = 1000)
rf.4
varImpPlot(rf.4)

rf.train.5 <- data.combined[1:891,c("Pclass","Title","family.size")]
set.seed(1234)
rf.5 <- randomForest(x=rf.train.5,y=rf.label,importance = TRUE,ntree = 1000)
rf.5
varImpPlot(rf.5)



rf.train.6 <- data.combined[1:891,c("Pclass","Title","SibSp","Parch","family.size")]
set.seed(1234)
rf.6 <-randomForest(x=rf.train.6,y=rf.label,importance = TRUE,ntree = 1000)
rf.6
varImpPlot(rf.6)


rf.train.5 <- data.combined[1:891,c("Pclass","Title","family.size")]
set.seed(1234)
rf.5 <- randomForest(x=rf.train.5,y=rf.label,importance = TRUE,ntree = 1000)
rf.5
varImpPlot(rf.5)

###################################################################################
###########################  1st submit  #################################
###################################################################################

test.submit.df <- data.combined[892:1309,c("Pclass","Title","family.size")]
rf.5.predict <-predict(rf.5,test.submit.df) 
summary(rf.5.predict)
submit.df <- data.frame(PassengerID = rep(892:1309),Survived = rf.5.predict)
write.csv(submit.df,file = "titanic.solution.01.csv",row.names = FALSE)


###################################################################################
###########################  cross validation  #################################
###################################################################################

