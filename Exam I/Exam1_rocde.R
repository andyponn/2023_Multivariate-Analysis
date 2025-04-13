##### Multivariate Analysis Exam 1 #####
### Student ID: 107508006
### Department: 歐西四 
### Name      : 陳葳芃

## Question 1
#read in the mtcars data set in R and name it "data".
data<-mtcars                             

#present a few descriptive statistics in a table 
#order: mean, median, standard deviation, minimum, maximum
rbind("Mean   :"=sapply(data, mean),      
      "Median :"=sapply(data, median),    
      "S.d.   :"=sapply(data, sd),
      "Min.   :"=sapply(data, min),
      "Max.   :"=sapply(data, max))

#column names which contains NA
names(which(colSums(is.na(data))>0))      
#amount of missing values in each column
colSums(is.na(data))                      


## Question 2
#impute missing values first
data<-na.omit(data)      
#calculate and present the correlation matrix of all variables
data_cor<-cor(data)                       
data_cor

## Question 3
str(data)
data_st<-as.data.frame(scale(data))
head(data_st)
data_st[,1] <- data[,1]
set.seed(69)
#amount of missing values in each column
colSums(is.na(data_st))                   

#切訓練及測試集（因考量到樣本數，故比例調為9 train:1 test）
train.index <- sample(x=1:nrow(data_st), size=ceiling(0.9*nrow(data_st) ))
train_df <- data_st[train.index, ]
test_df <- data_st[-train.index, ]
m.null = lm(mpg ~ 1, data = train_df)  
m.full = lm(mpg ~ ., data = train_df) 

#模型變數挑選stepwise regression analysis 
forward.lm = step(m.null, scope=list(lower=m.null, upper=m.full), direction="forward")
summary(forward.lm)
backward.lm = step(m.full, scope = list(upper=m.full), direction="backward") 
summary(backward.lm)
#兩個模型挑選結果均為lm(mpg ~ hp + wt, data = train_df)

#比較各模型（full model, forward.test, backward.test）與真實值的結果
v1<-test_df[rownames(test_df),1]
Q<-matrix(data=v1,ncol = nrow(test_df),nrow=1,byrow=TRUE)
colnames(Q)=rownames(test_df)
rownames(Q)="Actual value"
Q<-as.table(Q)

predict_result<-rbind( "lm.test" = predict(m.full, test_df),    
                       "forward.test" = predict(forward.lm, test_df),  
                       "backward.test" = predict(backward.lm, test_df),Q)
t(predict_result)


## Question 4
#簡易check回歸假設的方式
plot(forward.lm)
#常態大致符合V(但在head tail的表現不是非常理想)
#變異數同質性V()
#獨立性V(大致在0附近隨機跳動)

#perform normality tests
residuals <- forward.lm$residuals        
qqnorm(residuals)
qqline(residuals)
shapiro.test(residuals)
#結果顯示，p值為 0.05938，因此我們無法拒絕常態分配假設

#perform homoscedasticity tests
library(car)                             
leveneTest(residuals ~ as.factor(train_df$gear), data = train_df)
#結果顯示，p值為0.8669，因此我們無法拒絕變異數同質性假設。

#perform independence tests
library(lmtest)                          
dwtest(forward.lm)
#結果顯示，p值為0.817，因此我們無法拒絕獨立假設。


#如何解決heteroscedasticity 之問題？
#調整變數權數、重新定義應變數(y)、對應變數(y)作轉換
#here is an example if we choose to do the Box-Cox transformation
library(MASS)
bc=boxcox(forward.lm, lambda=seq(-3,3))
best.lm=bc$x[which(bc$y==max(bc$y))]
new.forward.lm= lm((mpg)^(best.lm)~hp + wt, data=train_df)
plot(new.forward.lm)
