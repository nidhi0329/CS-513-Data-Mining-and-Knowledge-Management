###############################################
##Name- Nidhi chovatiya
##CWID - 10457344
##lab-4
##Class- CS-513-A
###############################################



rm(list=ls())

breast_cancer <- read.csv("C:/Users/Nidhi/Desktop/513/lab2/breast-cancer-wisconsin.data.csv",na.string = "?",colClasses=c("Class"="factor"))
View(breast_cancer)

##To remove missing values 
DataF <- data.frame(breast_cancer)
removed_data <- na.omit(DataF)

#Converting the type of column F6 from character to numeric
removed_data$F6<-as.integer(removed_data$F6)

##factor the class row
?factor
removed_data$Class <- factor(removed_data$Class,labels = c("begin" ,"malignant"))


#70% of the sample size
idx <- floor(0.70 * nrow(removed_data))

#Set the seed to make your partition reproducible
set.seed(123)
index <- sample(seq_len(nrow(removed_data)), size = idx)

training <- removed_data[index, ]
test <- removed_data[-index, ]

#import required library
install.packages('e1071', dependencies = TRUE)
install.packages(class)
library(e1071)
library(class)

#Implementing NaiveBayes
modelnaive<- naiveBayes(Class ~ ., data = training)

#Predicting target class for the Validation set
predictnaive <- predict(modelnaive, test)

table(modelnaive=predictnaive,class=test$Class)

#prop table
prop.table(table(modelnaive=predictnaive,class=test$Class))

#Error in perdiction of result
wrongprediction<-sum(predictnaive!=test$Class)

#Error Rate in prediction of Naive Bayes Classifier
wrongpredictionrate<-wrongprediction/length(predictnaive)

#Output using naive bayes classification model
print(paste("Total bad Predictions:" , wrongprediction))
print(paste("Error rate :" , wrongpredictionrate))
print(paste("Accuracy :" , 100-(wrongpredictionrate*100)))

