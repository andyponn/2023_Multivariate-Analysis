##### 時間數列分析 期中報告
##### 組員：陳葳芃 賴冠維
##### 選用資料：台灣交通資料
##### 資料來源：

library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
traffic_data <- read_csv("Desktop/1112-NCCU/多變量分析/多變量報告期末數據/traffic.csv")
#traffic_data<-t(traffic_data)
#write.csv(traffic_data, "tdf.csv", row.names=T) 

#head(traffic_data)
traffic_data<-traffic
##########資料前處理——變更欄名稱
names(traffic_data)[27:36] = c('小型車數', '大客車數', '大貨車數', '全聯結車數', '半聯結車數',
                               '機車數', '合計車輛數', '總流量PCU','總計車公里','尖峰小時交通量PCU')
names(traffic_data)[c(16,17,19:26)] = c('里程km', '路面寬度m', '快車道1寬度', '快車道2寬度', '快車道3寬度', 
                                        '快車道4寬度', '快車道5寬度','機慢車道1寬度', '機慢車道2寬度', '路肩寬度')

names(traffic_data)[c(6,7)] = c('經度','緯度')

cols<- c("縣市別","路線編號","地形")
traffic_data[cols] <- lapply(traffic_data[cols], factor) 

traffic_data = traffic_data[,-c(39,40)]
str(traffic_data)

##########資料前處理——將車道大小轉化為車道數量以利分析
num = c(27:36)                           ##轉化為數值
for(i in 1:length(num)){
  traffic_data[,num[i]] = as.numeric(traffic_data[,num[i]])
}
str(traffic_data)

sum(is.na(traffic_data))                 ##篩選出需要的變數
traffic = traffic_data[,c(1,6,7,15:36)]   

na_row = which(apply(is.na(traffic), 1, any))   ##去除含NA的列
traffic = traffic[-na_row,]
sum(is.na(traffic))

fast_lane = NULL     ##計算各車道數量
slow_lane = NULL
for(i in 1:nrow(traffic)){
  fast_lane = c(fast_lane,5 - sum(traffic[i,8:12]==0))
  slow_lane = c(slow_lane, 2 - sum(traffic[i,13:14]==0))
}

##新增車道總數量
traffic$快車道數量 <- fast_lane
traffic$機慢車道數量 <- slow_lane

#traffic = traffic |> mutate(快車道數量 = fast_lane, 機慢車道數量 = slow_lane)
#traffic = traffic |> mutate(總車道數量 = 快車道數量+機慢車道數量)
###traffic[which(traffic[,3]>30),3]=22.7 

traffic1 = traffic

### choose numeric and needed columns, 1393~1490為東部資料
traffic=traffic[,-c(4,7,8:14,22)]
library(stats)
pairs(traffic)

##########資料分析——PCA主成分分析
pca.traffic = princomp(traffic, cor=T,)
summary(pca.traffic)

###permutation test
sign.pc<-function(x,R=1000,m=length(x), cor=T,...){
  pc.out<-princomp(x,cor=cor,...)
  pve=(pc.out$sdev^2/m)[1:m]
  pve.perm<-matrix(NA,ncol=m,nrow=R)
  for(i in 1:R){
    x.perm<-apply(x,2,sample)
    pc.perm.out<-princomp(x.perm,cor=cor,...)
    pve.perm[i,]=(pc.perm.out$sdev^2/m)[1:m]
  }
  pval<-apply(t(pve.perm)>pve,1,sum)/R
  return(list(pve=pve,pval=pval))
}
library(RCurl)
sign.pc(traffic,cor=T)
#
screeplot(pca.traffic, type = "l")
loadings(pca.traffic)

##3d圖
pcs.traffic = predict(pca.traffic)
pcs.traffic
library(scatterplot3d)
s3d <- scatterplot3d(pcs.traffic[,1:3],
                     pch = 15,
                     color = c('#FC4E07'),
                     lty.hide = 2 
)
##biplot
# install.packages("factoextra")
# library(factoextra)
# fviz_pca(pca.traffic)
#fviz_pca_var(pca.traffic)
# fviz_pca_ind(pca.traffic)

##########資料分析——FA因素分析
library(psych)
traffic.cor = cor(traffic)
traffic.cor
traffic.per = fa.parallel(traffic.cor, n.obs = 1490, fa="both")
traffic.fa = factanal(traffic, factors = 5, rotation = 'varimax', socres = "regression")
traffic.fa
#######
fa = factanal(traffic,factors=5,scores="regression")
plot(fa$scores[,1],fa$scores[,2],type="n", ylab='Factor 2',xlab= 'Factor 1')
text(fa$scores[,1:2],row.names(traffic),cex=0.8)
abline(h=0, v=0,col=2)


##########資料分析——DA區別分析
traffic.DA <- traffic1
traffic.DA                              

##check the data，進行地區分別
which(traffic.DA[,3]>24.70328)            ##新竹縣以北
which(23.5311< traffic.DA[,3]< 24.70328)  ##新竹縣以南 嘉義縣以北 

location = NULL                           ##區域類別 1:南, 2:中, 3:北
location.fa = NULL
for(i in 1:nrow(traffic.DA)){
  if(traffic.DA[i,3]>24.70328){
    location[i] = '北'
    location.fa[i] = 3
  } else if(traffic.DA[i,3]<24.70328&traffic.DA[i,3]>23.5311){
    location[i] = '中'
    location.fa[i] = 2
  } else{
    location[i] = '南'
    location.fa[i] = 1
  }
}

##選擇DA所需要用的資料
traffic.DA = traffic.DA |> mutate(區域 = location, area = location.fa)
traffic.DA = traffic.DA[,c(5,6,15:21,23:27,30)]
traffic.DA[,15] = traffic.DA[,15] |> as.factor()
str(traffic.DA)

set.seed(123)
index = sort(sample(nrow(traffic.DA), 0.8*nrow(traffic.DA)))
train = traffic.DA[index,]
test = traffic.DA[-index,]

##進行區別分析(lda)
library(MASS)
library(dplyr)
train = train |> as.data.frame()
model = lda(area~., data = train)




model = lda(Region~., data = train)

##dependent variable group means: X4 region
QQ<-Wilks.test(traffic.DA[,1:14], grouping=traffic.DA$area)
table<-QQ$estimate

QQ$call
library(biotools)
greedy.wilks(area~.,data=train)

greedy.wilks(Region~Quality,data=HBATN)
greedy.wilks(Region~Ecommerce,data=HBATN)
greedy.wilks(Region~Technical,data=HBATN)
greedy.wilks(Region~Flexibility,data=HBATN)


#############################
library(GenAlgo)

for (i in 6:18){
  data1<-data[,i]     #data[,i]放關心的each independent variable
  print(i)
  print(maha(data1, data$Region))       ##data$Region 分群變數
}

for (i in 1:13){
  a<-maha(data1[,i], groups=data1$Region, method = "var")
  b<-maha(data1[,i], groups=data1$Region, method = "mve")
  print(a)
  print(b)
}
#############################


#############################
#問為什麼這樣會出問題
DD<-data1[,c(1,14)]
distance<-mahalanobis(DD, colMeans(DD), cov(DD))
min(distance)
#############################

