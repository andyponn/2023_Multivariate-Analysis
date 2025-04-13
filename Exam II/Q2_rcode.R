##### Multivariate Analysis Exam 2-2 #####
### Student ID: 107508006
### Department: 歐西四 
### Name      : 陳葳芃

## Question 2 svm


### SVM ###
olive$index =c(1:nrow(olive))
train_df <- olive %>% group_by(Region) %>% sample_frac(0.8)
test_df  <- anti_join(olive, train_df, by = 'index')
traind <-as.data.frame(train_df[,-c(2,11)])
testd <-as.data.frame(test_df[,-c(2,11)])


library(e1071)
s <- svm(Region ~ Eicosenoic+Oleic, data = traind, probability = TRUE)
results <- predict(s, testd, probability = TRUE)
table(Real = testd$Region, Predict = results)



# Draw Data and Decision Boundary
#install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()

rcols <- palette(brewer.pal(n = 3, name = "Set3"))
plot(s, traind, Eicosenoic~Oleic,
    slice = list(Eicosenoic = 3, Oleic = 4),col = rcols,pch=16,bg=rols)

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

points(7.2, 2, pch = 21, col = "red", bg = "red")
text(7.5, 2, "Region1")
points(7.2, 2.2, pch = 21, col = "green", bg = "green")
text(7.5, 2.2, "Region2")
points(7.2, 2.4, pch = 21, col = "blue", bg = "blue")
text(7.5, 2.4, "Region3")



