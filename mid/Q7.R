# Name : Nidhi Chovatiya
# CS 513 A
# Mid Term Examination
# CWID : 10457344


#Loading the data set 
rm(list=ls())
ChooseFile <- file.choose()
COVID19 <- read.csv(ChooseFile,header = TRUE, na.strings=' ')

## Omit the missing values from the data
COVID19<- na.omit(COVID19)

#Creating function for normalizing
mmnorm <- function(x){
  z<- (x-min(x))/(max(x)-min(x))
  return(z)
}
#Removing the first column
COVID19 <- COVID19[-1]

#Normalizing data and using cbind 
data_normalized <- cbind.data.frame(Age = mmnorm(COVID19[, 1]), 
                                    Exposure = mmnorm(COVID19[, 2]),
                                    MaritalStatus = mmnorm(as.numeric(COVID19[, 3])), 
                                    Cases = mmnorm(COVID19[, 4]),
                                    MonthAtHospital = mmnorm(COVID19[, 5]), 
                                    Infected = COVID19[, 6])

summary(COVID19)

#splitting the data in 30% test and 70% training data
idx<-sample(nrow(COVID19),.3*nrow(COVID19))
test<-COVID19[idx,]
training<-COVID19[-idx,] 

#KNN MODEL
library(kknn)

#USING K=5 
predict_knn <- kknn(formula = Infected~.,training,test,k=5,kernel = "rectangular")
model_predict <- fitted(predict_knn)
confusion_matrix_knn<-table(model_predict,test$Infected)
confusion_matrix_knn

#accuracy model
accuracy <- function(x){
  sum(diag(x))/(sum(rowSums(x)))*100
}
accuracy(confusion_matrix_knn)
