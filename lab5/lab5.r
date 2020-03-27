###############################################
##Name- Nidhi chovatiya
##CWID - 10457344
##lab-5
##Class- CS-513-A
###############################################

rm(list=ls()) 
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)

breast_cancer <- read.csv("C:/Users/Nidhi/Desktop/513/lab2/breast-cancer-wisconsin.data.csv")
View(breast_cancer)
breast_cancer$Class <- factor(breast_cancer$Class, levels = c(2,4), labels = c("Benign", "Malignant")) 
set.seed(111)

#Random_Index_Creation
idx<-sort(sample(nrow(breast_cancer),as.integer((.30*nrow(breast_cancer)))))
training_data <- breast_cancer[-idx,]
test_data =breast_cancer[idx,]

#Tree_Creation
dev.off()
Cartclass<- rpart( Class~., data =training_data )
summary(Cartclass)

#Plotting_Graph
rpart.plot(Cartclass)


predictcart<-predict( Cartclass ,test_data , type="class" )

#Creates_Frequency_Table
table(Actual=test_data[,11],CART=predictcart)
predictcart2<-predict(Cartclass,test_data)
str(predictcart2)
predictcart_cat<-ifelse(predictcart2[,1]<=.5,'Malignant','Benign')
table(Actual=test_data[,11],CART=predictcart_cat)

#PercentageAccuracy
match<- (test_data[,11]==predictcart)*100
acc<-sum(match)/length(match)
acc

#ErrorRate
error<- sum(test_data[,11]!=predictcart)
errorrate<-error/length(test_data[,11])
errorrate

library(rpart.plot)
prp(Cartclass)

#graph
fancyRpartPlot(Cartclass) 

