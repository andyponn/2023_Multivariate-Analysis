library(readr)
HBATN <- read_csv("1112-NCCU/多變量分析/Code and TA/HBATN.csv")
library(MASS)
data1<-HBATN[,c(7:19,5)]
attach(data)
names(data)
HBARN.lda1<-lda(Region~.,data=data1)

lda1.predict<-predict(HBARN.lda1)


library(GenAlgo)

for (i in 1:13){
  data2<-data1[,i]     #data[,i]放關心的each independent variable
  #print(i)
  print(maha(data2, data1$Region))       ##data$Region 分群變數
}



####上課
plot()
coef()
HBATN.lda2$svd



table(HBATN$Region,HBATN.lda1$class)
plot(HBAT.lda2.p$x)
point(HBAT.lda2.p$x)
##加顏色
plot(HBAT.lda2.p$x, col=as.numeric(HBATN.lda2$class))

Sx<-cov(data1)

D2 <- mahalanobis(data1, colMeans(data1[,1]), Sx )


library(mda)
confusion(HBAT$Coustomer,prediction(HBAT,lda2)$class)



