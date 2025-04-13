##MDA PLOT

model <- mda(width~快車道數量+總流量PCU, data = train.transformed)
model.mda <- mda(width~., data = train.transformed)
contour_data <- expand.grid(快車道數量 = seq(-10, 10, length = 300), 總流量PCU = seq(-10, 10, length = 300))
mda_predict <- data.frame(contour_data, y = as.numeric(predict(model, contour_data)))
mp+stat_contour(aes(x=快車道數量, y=總流量PCU, z=y), data=mda_predict)+geom_raster(data=mda_predict, aes(x=快車道數量, y=總流量PCU,fill = factor(y)),alpha=0.2,show.legend =FALSE)

##method 2 for mda(較正確)
library(MASS)
XXX<-train.transformed[c(14,11,16)]
XXX<-as.data.frame(XXX)
model <- mda(width~快車道數量+總流量PCU, data = XXX)
decisionplot_ggplot(model, XXX, class = "width")




### QDA PLOT
qp = ggplot(train.transformed, aes(x = 快車道數量, y = 尖峰小時交通量PCU, color = width))+ geom_point()+labs(x="QDA1",y='QDA2')
qp+stat_contour(aes(x=快車道數量, y=尖峰小時交通量PCU, z=y),col= "Black", data=qda_predict)
library(MASS)
XXX<-train.transformed[c(13,14,16)]
XXX<-as.data.frame(XXX)
model <- qda(width~快車道數量+尖峰小時交通量PCU, data = XXX)
decisionplot_ggplot(model, XXX, class = "width")


### LDA PLOT
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



