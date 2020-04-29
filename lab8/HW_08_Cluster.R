###############################################
##Name- Nidhi chovatiya                      ##
##CWID - 10457344                            ##
##lab-7                                      ##
##Class- CS-513-A                            ##
###############################################

#8.1
rm(list=ls())

#load the file
breast_cancer<-read.csv("C:/Users/Nidhi/Desktop/SEM1/513/lab8/wisc_bc_ContinuousVar.csv",na.strings = '?')
View(breast_cancer)
summary(breast_cancer)
table(breast_cancer$diagnosis)

#factoring
breast_cancer<-na.omit(breast_cancer)
breast_cancer<-breast_cancer[-1]
cancerdist<-dist(breast_cancer[,-1])
hclustresult<-hclust(cancerdist)
plot(hclustresult)
hclust2<-cutree(hclustresult,2)
table(hclust2,breast_cancer[,1])


#8.2
rm(list=ls())

#load the file
breast_cancer<-read.csv("C:/Users/Nidhi/Desktop/SEM1/513/lab8/wisc_bc_ContinuousVar.csv",na.strings = '?')
View(breast_cancer)
summary(breast_cancer)
table(breast_cancer$diagnosis)

#To factor the data set
breast_cancer<-na.omit(breast_cancer)
breast_cancer<-breast_cancer[-1]
k_means<- kmeans(breast_cancer[,-1],2,nstart = 10)
k_means$cluster
table(k_means$cluster,breast_cancer[,1])

