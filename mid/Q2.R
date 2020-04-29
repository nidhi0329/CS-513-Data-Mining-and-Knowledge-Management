# Name : Nidhi Chovatiya
# CS 513 A
# Mid Term Examination
# CWID : 10457344


rm(list=ls())

covid <- read.csv("C:/Users/Nidhi/Desktop/513/mid/COVID19_v3.csv",na.string = "?")
View(covid)
d_frame<-data.frame(covid)

## midterm question 2.1->Summarizing each column 
summary(covid)

## midterm question 2.2->Identifying missing values
missing<- is.na(covid)
View(missing)

missing_values_Age <-which(is.na(covid$Age))
missing_values_Age

missing_values_MonthAtHospital <-which(is.na(covid$MonthAtHospital))
missing_values_MonthAtHospital

## midterm question 2.3->the frequency table of "Infected" vs. "MaritalStatus" 
freq_table <- table(covid$Infected,covid$MaritalStatus)
freq_table

## midterm question 2.4->the scatter plot of "Age", "MaritalStatus" and "MonthAtHospital"
dev.off()
pairs(d_frame[,c(2,4,6)],main = "Covid Graph")

## midterm question 2.5->box plots for columns:  "Age", "MaritalStatus" and "MonthAtHospital"
boxplot(covid[, c("Age", "MaritalStatus", "MonthAtHospital")], col = c('Green','Blue','Red'))
title("Box Plot")

## midterm question 2.6->VI.	Replacing the missing values of "Cases" with the "mean" of "Cases"
covid[is.na(covid)] = mean(covid$Cases, na.rm = TRUE)





