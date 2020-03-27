###############################################
##Name- Nidhi chovatiya
##CWID - 10457344
##lab-2
##subject - Knowledge discovery and data mining
##Class- CS-513-A
###############################################

rm(list=ls())

breast_cancer<- read.csv("C:/Users/Nidhi/Desktop/513/lab2/breast-cancer-wisconsin.data.csv",na.string = "?")
df<-data.frame()

?kknn()

missing_data<-na.omit(breast_cancer)
View(missing_data)

missing_data$Class<-factor(missing_data$Class, labels = c("begin","malignant"))
View(missing_data$Class)

idx<-sort(sample(nrow(missing_data),as.integer(.70*nrow(missing_data))))

training<-missing_data[idx,]
View(training)

test<-missing_data[-idx,]
View(test)

summary(test)
summary(training)
library(kknn)
library(class) 

predict_k3 <- kknn(formula=Class~., training, test, k=3,kernel ="rectangular")
predict_k5 <- kknn(formula=Class~., training, test, k=5,kernel ="rectangular")
predict_k10 <- kknn(formula=Class~., training, test, k=10,kernel ="rectangular")


fit3 <- fitted(predict_k3)
table(test$Class,fit3)

fit5 <- fitted(predict_k5)
table(test$Class,fit5)

fit10 <- fitted(predict_k10)
table(test$Class,fit10)

