###############################################
##Name- Nidhi chovatiya                      ##
##CWID - 10457344                            ##
##lab-7                                      ##
##Class- CS-513-A                            ##
###############################################

#6.1
rm(list = ls())
bc_c50 <- read.csv("C:/Users/Nidhi/Desktop/SEM1/513/lab6/breast-cancer-wisconsin.data.csv")
View(bc_c50)

#random index creation
idx<-sort(sample(nrow(bc_c50),as.integer((.70*nrow(bc_c50)))))
training_data<-bc_c50[idx,]
testing_data<-bc_c50[-idx,]

#install.packages("C50")
library(C50)
c50class<-C5.0(factor(Class)~.,data=training_data)
summary(c50class)
plot(c50class)


prediction<-predict(c50class,testing_data,type="class")

#creates frequency table
table(actual=testing_data[,11],prediction)

#percentage accuracy
match<-(testing_data[,11]==prediction)*100
acc<-sum(match)/length(match)
acc

#error-rate
error<-(testing_data[,11]!=prediction)
error_rate<-sum(error)/length(error)
error_rate


#6.2
rm(list=ls())

#load the library
library(randomForest)

bc_randomForest<-read.csv("C:/Users/Nidhi/Desktop/SEM1/513/lab6/breast-cancer-wisconsin.data.csv",na.strings = '?')
View(bc_randomForest)

#factoring
bc_randomForest$Class <- factor(bc_randomForest$Class, levels = c(2,4),labels = c("Benign", "Malignant"))
bc_randomForest<-na.omit(bc_randomForest)

#dividing test and training data
index<-sort(sample(nrow(bc_randomForest),as.integer(.70*nrow(bc_randomForest))))
trainingdataset<-bc_randomForest[index,]
testdataset<-bc_randomForest[-index,]

dev.off()
fit <- randomForest( Class~., data=trainingdataset, importance=TRUE, ntree=1000)
importance(fit)
varImpPlot(fit)
prediction<- predict(fit, testdataset)
table(actual=testdataset$Class,prediction)

#find the error rate
wrongdata<- (testdataset$Class!=prediction)
error<-sum(wrongdata)/length(wrongdata)
error 

