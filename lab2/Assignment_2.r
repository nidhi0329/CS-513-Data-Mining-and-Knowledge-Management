###############################################
##Name- Nidhi chovatiya
##CWID - 10457344
##lab-2
##subject - Knowledge discovery and data mining
##Class- CS_513-A
###############################################

rm(list=ls())

breast_cancer <- read.csv("C:/Users/Nidhi/Desktop/513/lab2/breast-cancer-wisconsin.data.csv",na.string = "?")
View(breast_cancer)

##query 1.1
summary(breast_cancer)


##query 1.2
d_frame<-data.frame(breast_cancer)
new<-is.na(d_frame)
View(new)


#query 1.3
for(i in 1:ncol(d_frame)){
  d_frame[is.na(d_frame[,i]), i] <- mean(d_frame[,i], na.rm = TRUE)
}
View(d_frame)


#query 1.4
d_frame[,c(11,7)]


#query 1.5
pairs(d_frame[,2:7])


#query 1.6
boxplot(d_frame[8:10])
hist(d_frame$F7)
hist(d_frame$F8)
hist(d_frame$F9)


#query 2

rm(list = ls())
breast_cancer <- read.csv("C:/Users/Nidhi/Desktop/513/lab2/breast-cancer-wisconsin.data.csv",na.string = "?")
View(breast_cancer)
d_frame<-data.frame(breast_cancer)
na.omit(d_frame)

