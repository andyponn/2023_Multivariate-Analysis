train
test

# 資料前處理，中心化標準化
library(caret)
library(lattice)
preproc.param <- train %>% 
  preProcess(method = c("center", "scale"))

train.transformed <- preproc.param %>% predict(train)
test.transformed <- preproc.param %>% predict(test)
df<-train.transformed

library(MASS)
# Fit the model

greedy.wilks(aim~.,data=train.transformed )
model <- lda(aim~., data = train.transformed)
# Make predictions
predictions <- model %>% predict(test.transformed)
# Model accuracy
mean(predictions$class==test.transformed$aim)

plot(model)
predictions <- model %>% predict(test.transformed)
names(predictions)



#####################
# Predicted classes
head(predictions$class, 6)
# Predicted probabilities of class memebership.
head(predictions$posterior, 6) 
# Linear discriminants
head(predictions$x, 3) 

lda.data <- cbind(train.transformed, predict(model)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = aim))

mean(predictions$class==test.transformed$aim)
