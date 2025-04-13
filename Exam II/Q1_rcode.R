##### Multivariate Analysis Exam 2 #####
### Student ID: 107508006
### Department: 歐西四 
### Name      : 陳葳芃

# Question 1 k means ####

olive <- read.table("~/Desktop/1112-NCCU/多變量分析/Code and TA/小考/olive.txt", header=TRUE, quote="\"")
which(is.na(olive))
str(olive)
summary(olive)

olive$Region<-as.factor(olive$Region)
olive$Area<-as.factor(olive$Area)

library(factoextra)
library(NbClust)
# Elbow method 6 根據每個資料點的分散以及聚合來衡量分群的結果
df<-olive[,-c(1,2)]
df<-scale(df)

#3
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2) + # add line for better visualisation
  labs(subtitle = "Elbow method") # add subtitle


#5
fviz_nbclust(df, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

#3
library(NbClust)
NbClust(data = df, distance = "euclidean", min.nc = 2, max.nc = 9, method = "kmeans", index = "all", alphaBeale = 0.1)

# Gap statistic 9
set.seed(42)
fviz_nbclust(df, kmeans,
             nstart = 25,
             method = "gap_stat",
             nboot = 500
) + # reduce it for lower computation time (but less precise results)
  labs(subtitle = "Gap statistic method")


library(cluster)
set.seed(42)
km_res <- kmeans(df, centers = 5, nstart = 20)
sil <- silhouette(km_res$cluster, dist(df))
fviz_silhouette(sil)
library(factoextra)
fviz_cluster(km_res, df, ellipse.type = "norm")

pca_result <- prcomp(df, scale = TRUE)
summary(pca_result)
# Question 2 svm ####
library(tidyverse)
olive$index =c(1:nrow(olive))

olive[,c(3:10)]<-scale(olive[,c(3:10)])

train_df <- olive %>% group_by(Region) %>% sample_frac(0.8)
test_df  <- anti_join(olive, train_df, by = 'index')
traind <-as.data.frame(train_df[,-c(2,11)])
testd <-as.data.frame(test_df[,-c(2,11)])
head(traind)

library(e1071)
s <- svm(Region ~ Linoleic+Eicosenoic, data = traind, probability = TRUE)
results <- predict(s, testd, probability = TRUE)
table(Real = testd$Region, Predict = results)

str(olive)
length(which(olive$Region=="1"))
length(which(olive$Region=="2"))
length(which(olive$Region=="3"))

col<-colnames(olive)
col<-col[-c(1,2,11)]
num_combinations <- choose(length(col), 2)
combinations <- combn(col, 2)

olive_color <- c(rep("red", 323), rep("green", 98), rep("blue", 151))
par(mfrow=c(2,2)) 
for (i in 1:ncol(combinations)) {
  element1 <- combinations[1, i]
  element2 <- combinations[2, i]
  
  plot(
    data = olive,
    x = olive[[element1]], 
    y = olive[[element2]],
    main = "plot",
    xlab = element1,
    ylab = element2,
    col = olive_color
  )
}

# Draw Data and Decision Boundary
#install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()

rcols <- palette(brewer.pal(n = 3, name = "Set3"))
plot(s, traind, Eicosenoic~Linoleic,
     slice = list(Eicosenoic = 100, Linoleic = 4),col = rcols)

plot(s, Eicosenoic~Linoleic, data=testd)

# Question 3 model comparison####

##model1
library(readr)
data <- read_csv("~/Desktop/1112-NCCU/多變量分析/Code and TA/2020-QS-World-University-Rankings-100_(1).csv")

str(data)
lm_data<-data[,-c(1,2)]
lm_data<-scale(lm_data)
lm_data<-as.data.frame(lm_data)

# 先把資料區分成 train=0.8, test=0.2 
set.seed(22)
train.index <- sample(x=1:nrow(lm_data), size=ceiling(0.8*nrow(lm_data) ))

train = lm_data[train.index, ]
test = lm_data[-train.index, ]

# 1.建立空的線性迴歸(只有截距項)
null = lm(Academic_Reputation ~ 1, data = train)  
full = lm(Academic_Reputation~ ., data = train) # 建立上界，也就是完整的線性迴歸

# 2.使用step()，一個一個把變數丟進去
forward.lm = step(null, 
                  scope=list(lower=null, upper=full), 
                  direction="forward")
summary(forward.lm)

model1 <- lm(Academic_Reputation ~ 0 + ., data = train)

library(DAAG)
cv_error <- cv.lm(data=test, model, m = 10)
# View the CV error
print(cv_error)

predictions <- predict(model1, test)
errors <- predictions - test$Academic_Reputation
rmse <- sqrt(mean(errors^2))
rmse

summary(model1)

##model2
# Load the necessary library
library(pls)

# Set the maximum number of components to consider
max_components <- 6

# Create an empty vector to store the RMSE values for each number of components
rmse_values <- numeric(max_components)

# Perform PCR with different numbers of components
for (i in 1:max_components) {
  model2 <- pcr(Academic_Reputation ~ . - 1, data = train, scale = TRUE, ncomp = i, validation = "CV")
  predicted <- predict(model2, newdata = test)
  rmse_values[i] <- sqrt(mean((predicted - test$Academic_Reputation)^2))
}


# View the RMSE values for different numbers of components
for (i in 1:max_components) {
  cat("Number of Components:", i, "RMSE:", rmse_values[i], "\n")
}


summary(model2)
validationplot(model2, val.type="RMSE")
validationplot(model2, val.type="R2")


##model3

library(pls)

# 建立PLS模型
model <- plsr(Academic_Reputation ~ . - 1, data = train, scale = TRUE, validation = "CV")

for (i in 1:max_components) {
  model3 <- plsr(Academic_Reputation ~ . , data = train, scale = TRUE, ncomp = i, validation = "CV", intercept = FALSE)
  predicted <- predict(model3, newdata = test)
  rmse_values[i] <- sqrt(mean((predicted - test$Academic_Reputation)^2))
}

for (i in 1:max_components) {
  cat("Number of Components:", i, "RMSE:", rmse_values[i], "\n")
}

validationplot(model3, val.type="RMSE")
validationplot(model3, val.type="R2")

summary(model3)

