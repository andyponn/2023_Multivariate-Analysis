##### 時間數列分析 期中報告
##### 組員：陳葳芃 賴冠維
##### 選用資料：台灣交通資料
##### 資料來源：

library(dplyr)
library(ggplot2)
library(stringr)
library(readr)

traffic <- read_csv("Desktop/1112-NCCU/多變量分析/traffic.csv")


traffic2<-traffic
traffic<-traffic2[,-1]

traffic<-traffic[,-c(2,3)]

library(psych)
traffic = scale(traffic)

traffic.cor = cor(traffic)
df.corr<-round(traffic.cor,3)
colnames(df.corr) <- paste0("X", 4:17)
rownames(df.corr) <- paste0("X", 4:17)
write.csv(df.corr, "df.corr.csv", row.names=T)

KMO.traffic = psych::KMO(traffic.cor)
round(KMO.traffic$ImCov,3)


diag(KMO.traffic$ImCov)
which(diag(KMO.traffic$ImCov)==min(diag(KMO.traffic$ImCov)))

#
colnames(traffic)
traffic.1 = traffic[,-12]
KMO(traffic.1)$ImCov |> diag()
which(diag(KMO(traffic.1)$ImCov)==min(diag(KMO(traffic.1)$ImCov)))

#尖峰小時交通量PCU
colnames(traffic.1)
traffic.2 = traffic.1[,-6]
KMO(traffic.2)$ImCov |> diag()
which(diag(KMO(traffic.2)$ImCov)==min(diag(KMO(traffic.2)$ImCov)))

#路面寬度m
colnames(traffic.2)
traffic.3 = traffic.2[,-12]
KMO(traffic.3)$ImCov |> diag()
which(diag(KMO(traffic.3)$ImCov)==min(diag(KMO(traffic.3)$ImCov)))

#大貨車數
colnames(traffic.3)
traffic.4 = traffic.3[,-4]
KMO(traffic.4)$ImCov |> diag()
which(diag(KMO(traffic.4)$ImCov)==min(diag(KMO(traffic.4)$ImCov)))

#半聯結車數
colnames(traffic.4)
traffic.5 = traffic.4[,-13]
KMO(traffic.5)$ImCov |> diag()
which(diag(KMO(traffic.5)$ImCov)==min(diag(KMO(traffic.5)$ImCov)))

##取到0.45 剩10個 
colnames(traffic.5)
traffic.6 = traffic.5[,-8]
KMO(traffic.6)$ImCov |> diag()
which(diag(KMO(traffic.6)$ImCov)==min(diag(KMO(traffic.6)$ImCov)))

##取到0.5 剩9個
colnames(traffic.6)
traffic.7 = traffic.6[,-6]
KMO(traffic.7)$ImCov |> diag()
which(diag(KMO(traffic.7)$ImCov)==min(diag(KMO(traffic.7)$ImCov)))


#取到0.5 剩8個
traffic.8 = traffic.7[,-7]
KMO(traffic.8)$ImCov |> diag()

write.csv(round(KMO(traffic.8)$ImCov,3), "KMO8.csv", row.names=T) 
##########################################################

pca<- prcomp(traffic, center = TRUE, scale = F)
names(pca) 
sum.pca<-summary(pca)
pca$sdev
write.csv(pca$sdev, "pca_sdev.csv", row.names=T) 

pca.factor <- pca$rotation
pca.factor[,1:4]

eigen<-eigen(cor(traffic))
plot(eigen$values,type="h")
abline(h=1, col="red") #Kaiser eigenvalue-greater-than-one rule, choose pc1~pc5 by Kaiser


pcs<-predict(pca)

traffic.8$位置<- traffic
plot(pcs[,1:2],type="n",xlab='1st PC',ylab='2nd PC')
text(pcs[,1:2], row.names(traffic.8), cex=0.7, family = "康熙字典體")
abline(h=0, v=0, col=2)


##########資料分析——FA因素分析
library(psych)
traffic.cor = cor(traffic.8)
traffic.cor
traffic.per = fa.parallel(traffic.cor, n.obs = 1490, fa="both")
traffic.fa = factanal(traffic.8, factors = 4, rotation = 'varimax', socres = "regression")
traffic.fa
#######
fa = factanal(traffic,factors=4,scores="regression")
plot(fa$scores[,1],fa$scores[,2],type="n", ylab='Factor 2',xlab= 'Factor 1')
text(fa$scores[,1:2],row.names(traffic.8),cex=0.8)
abline(h=0, v=0,col=2)


##########Factor Analysis

spearman.mle<-factanal(covmat=as.matrix(traffic.cor),factors=4)
names(spearman.mle)

spearman.mle2<-factanal(covmat=as.matrix(traffic.cor),factors=4,rotation = 'promax') 
#promax is better than varimax
#(as the result of the relationship between variables is related)

spearman.mle
1-spearman.mle$uniq

#Input data
factor.analysis <- factanal(traffic.8,factors=2,scores="Bartlett")
plot(factor.analysis$scores[,1],factor.analysis$scores[,2],type="n",xlab= 'Factor 1', ylab='Factor 2')
text(factor.analysis$scores[,1:2],row.names(traffic.8),cex=0.8)
abline(h=0, v=0,col=2)


fa4C <- fa(traffic,nfactors=3,rotate='varimax',fm='ml')
fa.diagram(fa4C)

print(fa4C$loadings,cutoff = 0.35)

fa4C$valid          # p. 140
fa4C$TLI

cor(fa4C$scores)     # p. 142


traffic.9 = traffic.8[,-8]
faC <- fa(traffic.9,nfactors=4,rotate='promax',fm='ml')
fa.diagram(fa4C)


