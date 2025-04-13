## Multivariate Analysis Midterm Report

library(readr)                              #input original data set
data <- read.csv("~/Desktop/1112-NCCU/多變量分析/多變量報告期末數據/newtaipei_data.csv")

str(data)                                   #observe the raw data and do some required transformation
as.character(data[,c(1,2)])
str(data[,2])

data["村里別"][data["村里別"] == ""] <- NA  #replace empty blank with NA
new_DF <- data[is.na(data$村里別),]         #to easier analyze, here we just select the administrative section only
new_DF$行政區別 = gsub("區","",new_DF$行政區別)
data <- na.omit(data)                       #remove NA (the completed administrative district data) row

df<- unite(data,行政區村里別,c("行政區別","村里別"),sep="_")
df <- data.frame(df, row.names = df[,1])
new_DF <- data.frame(new_DF, row.names = new_DF[,1])


library(tidyverse)                          #rename the column to make the analysis clearly and easier
df2<-df[,-1]
new_DF<-new_DF[,c(-1,-2)]
lookup <- c(降低火力發電 = "第7案", 降低燃煤發電 = "第8案",限制進口核食= "第9案", 婚姻一男一女 = "第10案", 禁童性平教育 = "第11案", 專法同性婚姻 = "第12案", 台灣正名運賽 = "第13案", 民法同性婚姻 = "第14案", 性平教育實施 = "第15案", 核能運轉延役 = "第16案")
new_DF<-rename(new_DF, all_of(lookup))

corr<-cor(new_DF)                           #observed the 
corr2<-as.data.frame(corr)
write_xlsx(corr2,"~/Desktop/corr2.xlsx")

########## PCA
pca<- prcomp(new_DF, center = TRUE, scale = TRUE)
names(pca) 
summary(pca)

install.packages("writexl")
library("writexl")
write_xlsx(new_DF,"~/Desktop/new_DF.xlsx")

pca.factor <- pca$rotation
pca.factor[,1:4]

eigen<-eigen(cor(df2))
plot(eigen$values,type="h")
abline(h=1, col="red") #Kaiser eigenvalue-greater-than-one rule, choose pc1~pc5 by Kaiser

pcs<-predict(pca)

plot(pcs[,1:2],type="n",xlab='1st PC',ylab='2nd PC')
text(pcs[,1:2], row.names(new_DF), cex=0.7, family = "康熙字典體")
abline(h=0, v=0, col=2)




##########Factor Analysis
sub <- cor(new_DF)
spearman.mle<-factanal(covmat=as.matrix(sub),factors=3)
spearman.mle<-factanal(covmat=as.matrix(sub),factors=3,rotation = 'promax') 
#promax is better than varimax
#(as the result of the relationship between variables is related)

spearman.mle
1-spearman.mle$uniq

#Input data
factor.analysis <- factanal(new_DF,factors=2,scores="Bartlett")
plot(factor.analysis$scores[,1],factor.analysis$scores[,2],type="n",xlab= 'Factor 1', ylab='Factor 2')
text(factor.analysis$scores[,1:2],row.names(new_DF),cex=0.8)
abline(h=0, v=0,col=2)






