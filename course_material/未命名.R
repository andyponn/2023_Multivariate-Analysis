##### 多變量分析 自我分析報告
##### 組員        ：陳葳芃 賴冠維
##### 選用資料    ：台灣交通資料
##### Part        ：使用「地形」來嘗試區別分析(DA)
##### Pre-required：traffic1 data
##### Code更新日期：2023.05.15 

library(dplyr)
library(ggplot2)
library(stringr)
library(readr)

traffic <- read_csv("~/Desktop/111-2多變量分析/traffic.csv")

##篩選出需要的變數，並將分類變數「地形」設為 aim，1=丘陵區,2=山嶺區,3=平原區

df = traffic[,c(4,2,3,5,6,15:21,23:27)]
df<-df[-c(1011,1012),] ##緯度這兩筆有問題，予以刪除

cols<- c("地形")
df[cols] <- lapply(df[cols], factor) 
levels(df$地形)
names(df)[1]<-"aim"



df$aim <- as.numeric(df$aim)
df$aim <- as.factor(df$aim)
levels(df$aim)



##先切training testing data
set.seed(123)
index = sample(1:nrow(df), 0.8*nrow(df))
train = df[index,]
test = df[-index,]

##計算樣本數
length(which(train$aim=="1")) #n1=165
length(which(train$aim=="2")) #n2=68
length(which(train$aim=="3")) #n3=957
165/nrow(train)
68/nrow(train)
957/nrow(train)

##取每組平均
library(rrcov)
sum(is.na(df))
QQ<-Wilks.test(df[,c(2:17)], grouping=df$aim)
table<-t(QQ$estimate)
table

str(train)

##計算lambda
library(klaR)
vars<-colnames(df)
vars<-vars[2:(length(vars))]

lambda<-c()
F.value<-c()
significance<-c()
for (var in vars) {
  print(var)
  formula <- paste("aim ~", var)
  print(as.formula(formula))
  model<-greedy.wilks(as.formula(formula), data=train)
  lambda[var]<-model$results$Wilks.lambda[1]
  F.value[var]<-model$results$F.statistics.overall[1]
  significance[var]<-model$results$p.value.overall[1]
  #summary(model)
}

table <- cbind(table, lambda)
table <- cbind(table, F.value)
table <- cbind(table, significance)

##計算分組 maha distance（分成3 Group再取最小，並看是從哪裡取出來的）

train<-train.transformed
train$aim<-as.character(train$aim)          ##這邊要先將原本的levels轉乘character，否則levels最後還是會算成3

train23 = train |>
  filter(aim != "1")
train13 = train |>
  filter(aim!= "2")
train12 = train |>
  filter(aim!= "3")   

train12$aim<-as.factor(train12$aim)        ##再將character轉成levels，分組計算maha distance
train13$aim<-as.factor(train13$aim)
train23$aim<-as.factor(train23$aim)

str(train12)
str(train13)
str(train23)

library(GenAlgo)
mahavalue12<-c()
for (i in 2:17){
  #print(i)
  a<-maha(train12[,i], groups=train12$aim, method = "var")
  #print(a)
  mahavalue12[i-1]<-as.numeric(a)
}

mahavalue13<-c()
for (i in 2:17){
  #print(i)
  a<-maha(train13[,i], groups=train13$aim, method = "var")
  #print(a)
  mahavalue13[i-1]<-as.numeric(a)
}

mahavalue23<-c()
for (i in 2:17){
  #print(i)
  a<-maha(train23[,i], groups=train23$aim, method = "var")
  #print(a)
  mahavalue23[i-1]<-as.numeric(a)
}

table <- cbind(table, mahavalue12)         
table <- cbind(table, mahavalue13)
table <- cbind(table, mahavalue23) 


table<-as.data.frame(table)
library(matrixStats)
table$maha_min<-rowMins(as.matrix(table[,c(12,13,14)]))

##找出是哪個group
for(i in 1:length(table$maha_min)) {
  if(table$maha_min[i] == mahavalue12[i]){
    table$between[i] ="1 and 2"
  } else if(table$maha_min[i] == mahavalue13[i]){
    table$between[i] ="1 and 3"
  } else if(table$maha_min[i] == mahavalue23[i]){
    table$between[i] ="2 and 3"
  } else{
    table$between[i] ="Na"
  }
}


table[1:4] <- round(table[1:4], 3)                         ##這一張就是簡易版的table 5
write.csv(table, "fortable寬度.csv", row.names=T)               ##方便資料匯出後續書面報告使用

#資料前處理，中心化標準化
library(caret)
library(lattice)
preproc.param <- train %>% 
  preProcess(method = c("center", "scale"))

train.transformed <- preproc.param %>% predict(train)
test.transformed <- preproc.param %>% predict(test)

##LDA
library(MASS)                                                #變數挑選標準（labmda, maha distance）
model.0 <- lda(aim~., data = train.transformed)              #建模（指標最優的3個進行線性組合）
model.1 <- lda(aim~里程km, data = train.transformed)
#model.2 <- lda(aim~路面寬度m, data = train.transformed)
#model.3 <- lda(aim~快車道數量, data = train.transformed)
model.4 <- lda(aim~里程km+路面寬度m, data = train.transformed)
model.5 <- lda(aim~里程km+快車道數量, data = train.transformed)
model.6 <- lda(aim~路面寬度m+快車道數量, data = train.transformed)
model.7 <- lda(aim~里程km+路面寬度m+快車道數量, data = train.transformed)


predictions.0 <- model.0 %>% predict(test.transformed)       #預測(放入測試集)
predictions.1 <- model.1 %>% predict(test.transformed)
predictions.2 <- model.2 %>% predict(test.transformed)
predictions.3 <- model.3 %>% predict(test.transformed)
predictions.4 <- model.4 %>% predict(test.transformed)
predictions.5 <- model.5 %>% predict(test.transformed)
predictions.6 <- model.6 %>% predict(test.transformed)
predictions.7 <- model.7 %>% predict(test.transformed)


mean(predictions.0$class==test.transformed$aim)              #Model accuracy (模型全放準確率87.91%)
mean(predictions.1$class==test.transformed$aim)              #(模型準確率83.55%)
mean(predictions.2$class==test.transformed$aim)              #(模型準確率81.20%)
mean(predictions.3$class==test.transformed$aim)              #(模型準確率80.87%)
mean(predictions.4$class==test.transformed$aim)              #(模型準確率84.22%) prefered V
mean(predictions.5$class==test.transformed$aim)              #(模型準確率83.89%)
mean(predictions.6$class==test.transformed$aim)              #(模型準確率81.20%)
mean(predictions.7$class==test.transformed$aim)              #(模型準確率84.22%)

##lda繪圖
library(MASS)
library(ggplot2)
library(scales)
fit <- lda(aim ~ ., data = train.transformed)
datPred <- data.frame(aim=predict(model.4)$class,predict(model.4)$x)



#Create decision boundaries
fit2 <- lda(aim ~ LD1 + LD2, data=datPred)
ld1lim <- expand_range(c(min(datPred$LD1),max(datPred$LD1)),mul=0.05)
ld2lim <- expand_range(c(min(datPred$LD2),max(datPred$LD2)),mul=0.05)

ld1 <- seq(ld1lim[[1]], ld1lim[[2]], length.out=300)
ld2 <- seq(ld2lim[[1]], ld2lim[[2]], length.out=300)
newdat <- expand.grid(list(LD1=ld1,LD2=ld2))
preds <-predict(fit2,newdata=newdat)
predclass <- preds$class
postprob <- preds$posterior
df <- data.frame(x=newdat$LD1, y=newdat$LD2, class=predclass)
df$classnum <- as.numeric(df$class)
df <- cbind(df,postprob)
colorfun <- function(n,l=65,c=100) { hues = seq(15, 375, length=n+1); hcl(h=hues, l=l, c=c)[1:n] } # default ggplot2 colours
colors <- colorfun(3)
colorslight <- colorfun(3,l=90,c=50)
lda.data <- cbind(test.transformed, predict(model.4,test.transformed)$x)

ggplot(lda.data, aes(LD1,LD2)) +
  geom_point(aes(color = aim))+
  geom_contour(data=df, aes(x=x, y=y, z=classnum), colour="red2", alpha=0.5, breaks=c(1.5,2.5))+
  geom_raster(data=df, aes(x=x, y=y, fill = factor(class)),alpha=0.2,show.legend =FALSE)


##train confusionM
confusionMatrix(factor(train.transformed$aim), factor(predict(model.0,train.transformed)$class))
##test confusionM
confusionMatrix(factor(test.transformed$aim), factor(predict(model.0,test.transformed)$class))


##train confusionM
confusionMatrix(factor(train.transformed$aim), factor(predict(model.4,train.transformed)$class))
##test confusionM
confusionMatrix(factor(test.transformed$aim), factor(predict(model.4,test.transformed)$class))


##QDA


library(dplyr)
library(mvnormalTest)
norm_df = train[, 2:17]
norm_df<-sample_n(norm_df,100)
mardia(norm_df)

qmodel.0 <- qda(aim~., data = train.transformed)              #建模（指標最優的3個進行線性組合）
qmodel.1 <- qda(aim~里程km, data = train.transformed)
qmodel.2 <- qda(aim~路面寬度m, data = train.transformed)
qmodel.3 <- qda(aim~快車道數量, data = train.transformed)
qmodel.4 <- qda(aim~里程km+路面寬度m, data = train.transformed)
qmodel.5 <- qda(aim~里程km+快車道數量, data = train.transformed)
qmodel.6 <- qda(aim~路面寬度m+快車道數量, data = train.transformed)
qmodel.7 <- qda(aim~里程km+路面寬度m+快車道數量, data = train.transformed)


q.predictions.0 <- qmodel.0 %>% predict(test.transformed)       #預測(放入測試集)
q.predictions.1 <- qmodel.1 %>% predict(test.transformed)
q.predictions.2 <- qmodel.2 %>% predict(test.transformed)
q.predictions.3 <- qmodel.3 %>% predict(test.transformed)
q.predictions.4 <- qmodel.4 %>% predict(test.transformed)
q.predictions.5 <- qmodel.5 %>% predict(test.transformed)
q.predictions.6 <- qmodel.6 %>% predict(test.transformed)
q.predictions.7 <- qmodel.7 %>% predict(test.transformed)


mean(q.predictions.0$class==test.transformed$aim)              #Model accuracy (模型全放準確率72.48%)
mean(q.predictions.1$class==test.transformed$aim)              #(模型準確率81.20%)
mean(q.predictions.2$class==test.transformed$aim)              #(模型準確率80.87%)
mean(q.predictions.3$class==test.transformed$aim)              #(模型準確率80.87%)
mean(q.predictions.4$class==test.transformed$aim)              #(模型準確率83.22%) 
mean(q.predictions.5$class==test.transformed$aim)              #(模型準確率82.88%)
mean(q.predictions.6$class==test.transformed$aim)              #(模型準確率84.22%) prefered V
mean(q.predictions.7$class==test.transformed$aim)              #(模型準確率84.22%) 

##QDA繪圖

qp = test.transformed |> 
  ggplot()+
  geom_point(aes(快車道數量, 路面寬度m,color=aim,shape=aim), size=2.5)+
  labs(x = 'amount of fast lane',
       y = 'width of road(m)')
contour_data <- expand.grid(快車道數量 = seq(-4, 8, length = 300), 路面寬度m = seq(-8, 8, length = 300))
qda_predict <- data.frame(contour_data, y = as.numeric(predict(qmodel.6, contour_data)$class))
qp+stat_contour(aes(x=快車道數量, y=路面寬度m, z=y), data=qda_predict)+geom_raster(data=qda_predict, aes(x=快車道數量, y=路面寬度m, fill = factor(y)),alpha=0.2,show.legend =FALSE)

##train confusionM
confusionMatrix(factor(train.transformed$aim), factor(predict(qmodel.0,train.transformed)$class))
##test confusionM
confusionMatrix(factor(test.transformed$aim), factor(predict(qmodel.4,test.transformed)$class))


##MDA
library(mda)
m.model.0 <- mda(aim~., data = train.transformed)              #建模（指標最優的3個進行線性組合）
m.model.1 <- mda(aim~里程km, data = train.transformed)
m.model.2 <- mda(aim~路面寬度m, data = train.transformed)
m.model.3 <- mda(aim~快車道數量, data = train.transformed)
m.model.4 <- mda(aim~里程km+路面寬度m, data = train.transformed,keep.fitted=T)
m.model.5 <- mda(aim~里程km+快車道數量, data = train.transformed)
m.model.6 <- mda(aim~路面寬度m+快車道數量, data = train.transformed)
m.model.7 <- mda(aim~里程km+路面寬度m+快車道數量, data = train.transformed)


m.predictions.0 <- m.model.0 %>% predict(test.transformed)       #預測(放入測試集)
m.predictions.1 <- m.model.1 %>% predict(test.transformed)
m.predictions.2 <- m.model.2 %>% predict(test.transformed)
m.predictions.3 <- m.model.3 %>% predict(test.transformed)
m.predictions.4 <- m.model.4 %>% predict(test.transformed)
m.predictions.5 <- m.model.5 %>% predict(test.transformed)
m.predictions.6 <- m.model.6 %>% predict(test.transformed)
m.predictions.7 <- m.model.7 %>% predict(test.transformed)


mean(m.predictions.0==test.transformed$aim)              #Model accuracy (模型全放準確率88.25%)
mean(m.predictions.1==test.transformed$aim)              #(模型準確率82.21%)
mean(m.predictions.2==test.transformed$aim)              #(模型準確率81.20%)
mean(m.predictions.3==test.transformed$aim)              #(模型準確率79.53%)
mean(m.predictions.4==test.transformed$aim)              #(模型準確率84.89%)  prefered V
mean(m.predictions.5==test.transformed$aim)              #(模型準確率80.20%)
mean(m.predictions.6==test.transformed$aim)              #(模型準確率81.20%) 
mean(m.predictions.7==test.transformed$aim)              #(模型準確率84.56%) 

##mda繪圖
mp = train.transformed |> 
  ggplot()+
  geom_point(aes(里程km, 路面寬度m,color=aim,shape=aim), size=2.5)+
  labs(x = 'amount of fast lane',
       y = 'width of road(m)')
contour_data <- expand.grid(里程km = seq(-4, 8, length = 300), 路面寬度m = seq(-8, 8, length = 300))
mda_predict <- data.frame(contour_data, y = as.numeric(predict(m.model.4, contour_data)))
mp+stat_contour(aes(x=里程km, y=路面寬度m, z=y), data=mda_predict)+geom_raster(data=mda_predict, aes(x=里程km, y=路面寬度m, fill = factor(y)),alpha=0.2,show.legend =FALSE)


##train confusionM
confusionMatrix(factor(train.transformed$aim), factor(predict(m.model.0,train.transformed)))
##test confusionM
confusionMatrix(factor(test.transformed$aim), factor(predict(m.model.0,test.transformed)))



##CDA(先忽略～～)
library(candisc)
traffic.mod <- lm(cbind(里程km,路面寬度m,快車道數量)~aim , data=train.transformed)
can.0 <- candisc(traffic.mod, data=train.transformed)



## FDA
library(mda)
f.model.0 <- fda(aim~., data = train.transformed)              #建模（指標最優的3個進行線性組合）
f.model.1 <- fda(aim~里程km, data = train.transformed)
f.model.2 <- fda(aim~路面寬度m, data = train.transformed)
f.model.3 <- fda(aim~快車道數量, data = train.transformed)
f.model.4 <- fda(aim~里程km+路面寬度m, data = train.transformed,keep.fitted=T)
f.model.5 <- fda(aim~里程km+快車道數量, data = train.transformed)
f.model.6 <- fda(aim~路面寬度m+快車道數量, data = train.transformed)
f.model.7 <- fda(aim~里程km+路面寬度m+快車道數量, data = train.transformed)


f.predictions.0 <- f.model.0 %>% predict(test.transformed)       #預測(放入測試集)
f.predictions.1 <- f.model.1 %>% predict(test.transformed)
f.predictions.2 <- f.model.2 %>% predict(test.transformed)
f.predictions.3 <- f.model.3 %>% predict(test.transformed)
f.predictions.4 <- f.model.4 %>% predict(test.transformed)
f.predictions.5 <- f.model.5 %>% predict(test.transformed)
f.predictions.6 <- f.model.6 %>% predict(test.transformed)
f.predictions.7 <- f.model.7 %>% predict(test.transformed)


mean(f.predictions.0==test.transformed$aim)              #Model accuracy (模型全放準確率87.91%)
mean(f.predictions.1==test.transformed$aim)              #(模型準確率83.55%)
mean(f.predictions.2==test.transformed$aim)              #(模型準確率81.20%)
mean(f.predictions.3==test.transformed$aim)              #(模型準確率80.87%)
mean(f.predictions.4==test.transformed$aim)              #(模型準確率84.22%)  prefered V
mean(f.predictions.5==test.transformed$aim)              #(模型準確率83.89%)
mean(f.predictions.6==test.transformed$aim)              #(模型準確率81.20%) 
mean(f.predictions.7==test.transformed$aim)              #(模型準確率84.22%) 


fp = test.transformed |> 
  ggplot()+
  geom_point(aes(里程km, 路面寬度m,color=aim,shape=aim), size=2.5)
contour_data <- expand.grid(里程km = seq(-4, 8, length = 300), 路面寬度m = seq(-8, 8, length = 300))
mda_predict <- data.frame(contour_data, y = as.numeric(predict(model, contour_data)))
mp+stat_contour(aes(x=里程km, y=路面寬度m, z=y,), data=mda_predict)+geom_raster(data=mda_predict, aes(x=里程km, y=路面寬度m, fill = factor(y)),alpha=0.2,show.legend =FALSE)


library(MASS)
XXX<-train.transformed[c(11,14,16)]
XXX<-as.data.frame(XXX)
model <- fda(width~快車道數量+總流量PCU, data = XXX)
decisionplot_ggplot(model, XXX, class = "width")   

















##
library(MASS)
library(ggplot2)
library(scales)
fit <- lda(aim ~ ., data = train.transformed)
datPred <- data.frame(aim=predict(fit)$class,predict(fit)$x)
#Create decision boundaries
fit2 <- lda(aim ~ LD1 + LD2, data=datPred)
ld1lim <- expand_range(c(min(datPred$LD1),max(datPred$LD1)),mul=0.05)
ld2lim <- expand_range(c(min(datPred$LD2),max(datPred$LD2)),mul=0.05)
ld1 <- seq(ld1lim[[1]], ld1lim[[2]], length.out=300)
ld2 <- seq(ld2lim[[1]], ld1lim[[2]], length.out=300)
newdat <- expand.grid(list(LD1=ld1,LD2=ld2))
preds <-predict(fit2,newdata=newdat)
predclass <- preds$class
postprob <- preds$posterior
df <- data.frame(x=newdat$LD1, y=newdat$LD2, class=predclass)
df$classnum <- as.numeric(df$class)
df <- cbind(df,postprob)
colorfun <- function(n,l=65,c=100) { hues = seq(15, 375, length=n+1); hcl(h=hues, l=l, c=c)[1:n] } # default ggplot2 colours
colors <- colorfun(3)
colorslight <- colorfun(3,l=90,c=50)

lda.data <- cbind(train.transformed, predict(model.0)$x)
ggplot(lda.data, aes(LD1,LD2))+
  geom_point(aes(color = aim))+
  geom_contour(data=df, aes(x=x, y=y, z=classnum), colour="red2", alpha=0.5, breaks=c(1.5,2.5))+
  geom_raster(data=df, aes(x=x, y=y, fill = factor(class)),alpha=0.2,show.legend =FALSE)          ##色塊塗色



#################boundaries plot(mda)


m.model.0 <- mda(aim~ ., data = train.transformed,keep.fitted=T) ##

m.model.4 <- mda(width~快車道數量+總流量PCU, data = train.transformed,,keep.fitted=T)

m.model.4$fit$fitted.values %>% 
  as_tibble() %>% 
  bind_cols(aim = train.transformed[,1]) %>% 
  ggplot() +
  geom_point(aes(V1, V2, color = aim, shape = aim), size = 2.5) + 
  labs(x = paste("MDA1 (", percent(m.model.0$percent.explained[1]/100), ")", sep=""),
       y = paste("MDA2 (", percent(m.model.0$percent.explained[2]/100 - m.model.4$percent.explained[1]/100), ")", sep=""))
#geom_contour(data=df, aes(x=x, y=y, z=classnum), colour="red2", alpha=0.5, breaks=c(-2,2.5))


p <- ggplot(train_data, aes(x = 里程km, y = 路面寬度m, color = y)) + 
  geom_point()
p + stat_contour(aes(x = 里程km, y = 路面寬度m, z = y), data = mda_predict) + 
  ggtitle("MDA Decision Boundaries")





# NOT RUN {
data(iris)
model <- fda(width~快車道數量+總流量PCU, data = XXX)
plot(irisfit)
data(ESL.mixture)
## Not a data frame
mixture.train=ESL.mixture[c("x","y")] 
mixfit=mda(y~x, data=mixture.train)
plot(mixfit, mixture.train)
plot(mixfit, data=ESL.mixture$xnew, group="pred")