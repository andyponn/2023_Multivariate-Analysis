##計算maha distance
library(GenAlgo)

train$aim<-as.character(train$aim)

train23 = train |>
  filter(aim != "1")
train13 = train |>
  filter(aim!= "2")
train12 = train |>
  filter(aim!= "3")   

train12$aim<-as.factor(train12$aim)
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
table <- round(table, 3)


table<-as.data.frame(table)
m<-as.matrix(table[,c(7,8,9)])
table$maha_min<-rowMins(m)





for(i in 1:length(table$maha_min)){
  if(table$maha_min[i] == mahavalue12[i]){
    table$between[i] =="1 and 2"
  } else if(table$maha_min[i] == mahavalue13[i]){
    table$between[i] =="1 and 3"
  } else if(table$maha_min[i] == mahavalue23[i]){
    table$between[i] =="1 and 3"
  } else{
    table$between[i] =="Na"
  }
  
  