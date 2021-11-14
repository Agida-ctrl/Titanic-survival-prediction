##### Titanic Survival Prediction ########
#=====================================#

library(caret)
library(kernlab)
library(dplyr)

train<-read.csv(file.choose(),
                header = T,sep = ",",
                na.strings =c("NA"," ","") ,
                stringsAsFactors = F)

test<-read.csv(file.choose(),
               header = T,sep = ",",
               na.strings =c("NA"," ","") ,
               stringsAsFactors = F)

str(train)
str(test)

summary(train)
attach(train)



table(train$Embarked,useNA = "always")
train$Embarked[is.na(train$Embarked=="NA")]<-"S"
sum(is.na(train$Age)==TRUE)/length(train$Age)
train$Age[is.na(train$Age=="NA")]<-mean(Age,na.rm = T)

anyNA(train)

train<-train%>%
  select(-c(PassengerId,Name,
                  Cabin,Ticket,))%>%
  mutate(relative=SibSp+Parch)


train$Survived<-as.factor(train$Survived)
train$Pclass<-as.factor(train$Pclass)
train$Sex<-as.factor(train$Sex)
train$Embarked<-as.factor(train$Embarked)

index<-createDataPartition(train$Survived,p=.7,times = 1,list = F)

trainY<-train[index,]
testY<-train[-index,]
####==================================####

library(kernlab)
trainDist <- sigest(Survived ~ ., data = train, frac = 1)
svmTuneGrid <- data.frame(sigma = as.vector(trainDist)[1], C = 2^(-2:7))


svmbootcv<-train(Survived ~ .,
                           data = trainset,
                           method = "svmRadial",
                           preProc = c("center", "scale"),
                           tuneGrid = svmTuneGrid,
                           trControl = trainControl(method = "boot", 
                                                    number = 10))

plot(svmbootcv, scales = list(x = list(log = 2)))

predboot<-predict(svmbootcv,testY)

confusionMatrix(predboot,testY$Survived)

saveRDS(svmbootcv, "modelsvm.rds")
svm<-readRDS("modelsvm.rds")

##New dataset


summary(test)

test<-test%>%
  select(-c(Name,
            Cabin,Ticket,))%>%
  mutate(relative=SibSp+Parch)

test$Pclass<-as.factor(test$Pclass)
test$Sex<-as.factor(test$Sex)
test$Embarked<-as.factor(test$Embarked)
sum(is.na(test$Age)==TRUE)/length(test$Age)
test$Age[is.na(test$Age=="NA")]<-mean(Age,na.rm = T)
test$Fare[is.na(test$Fare=="NA")]<-mean(Fare,na.rm = T)
anyNA(test)
summary(test)
predTest<-predict(svm,test)

predictions<-data.frame(PassengerId=test$PassengerId,
                        Survived=predTest)

write.csv(predictions,"C:\\Users\\TM\\Desktop\\Agida\\Data science\\new dataset\\submissions\\Agida.csv",row.names = FALSE)
