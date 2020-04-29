###############################################
##Name- Nidhi chovatiya                      ##
##CWID - 10457344                            ##
##lab-7                                      ##
##Class- CS-513-A                            ##
###############################################

rm(list=ls())

#load the library
library(neuralnet)

breast_cancer<-read.csv("C:/Users/Nidhi/Desktop/SEM1/513/lab7/wisc_bc_ContinuousVar.csv",na.strings = '?')
View(breast_cancer)

breast_cancer<-data.frame(lapply(na.omit(breast_cancer),as.numeric))

table(breast_cancer$diagnosis)

#splitting data
index<-sort(sample(nrow(breast_cancer),as.integer(.70*nrow(breast_cancer))))
trainingdataset<-breast_cancer[index,]
testdataset<-breast_cancer[-index,]
dev.off()
?neuralnet()
model<- neuralnet(diagnosis~.,trainingdataset[-1], hidden=5, threshold=0.01)

#Plotting neural network
plot(model)


ann <-compute(model,testdataset)
ann$net.result

anncat<-ifelse(ann$net.result <1.5,1,2)
length(anncat)
length(testdataset$diagnosis)
table(anncat,testdataset$diagnosis)

#finding error rate
wrong<- (testdataset$diagnosis!=anncat)
error_rate<-sum(wrong)/length(wrong)
error_rate

