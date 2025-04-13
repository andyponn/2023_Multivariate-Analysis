write.csv(table, "table.csv")

library(readr)
full_data <- read_csv("Desktop/1112-NCCU/多變量分析/full_data.csv")

str(full_data)
full_data<-full_data[,-1]

##可讀性命名英文
colnames(full_data) <- c("county", "inves_number", "place", "inves_knumber", "inves_100number",
                         "longitude","latitude","route","start","destination",
                         "start_knumber","start_100number","destination_knumber","destination_100number","landform",
                         "mile(km)","road_width(m)","direction","flane1_width(m)","flane2_width(m)",
                         "flane3_width(m)","flane4_width(m)","flane5_width(m)","slane1_width(m)","slane2_width(m)",
                         "shoulder_width(m)","scar_amount","bus_amount","truck_amount","ftrailer_amount",
                         "strailer_amount","scooter_amount","sum_amount","totalPCU","total_km",
                         "rushPCU","rush_start","rush_finish","coeffient","note")
##車道數量
fast_lane = NULL
slow_lane = NULL
for(i in 1:nrow(full_data)){
  fast_lane = c(fast_lane,5 - sum(full_data[i,19:23]==0))
  slow_lane = c(slow_lane, 2 - sum(full_data[i,24:25]==0))
}
full_data$lane_amount <- fast_lane+slow_lane
str(full_data)

df<-full_data[,-c(19:25)]

odd<-df[seq(1,nrow(df),2),]
even<-df[seq(0,nrow(df),2),]


##try

ldaHmat(x=df[,c(4,5,11:14,16,17)], grouping=df$landform)

colname(HBATN)
aa<-manova( cbind(HBATN$Quality,HBATN$Ecommerce,HBATN$Technical)~Region,data=HBATN)
colnames(HBATN)

summary(aa,test="W")
model <- lda(Region ~ HBATN$Quality, data = HBATN)
now<-HBATN[,c(7:19,5)]

colnames(HBATN)
attach(now)
Region<-factor(Region)
str(now$Region)
now$Region<-as.factor(now$Region)
now.mod=lm(cbind(Quality,Ecommerce,Technical,Complaint,Advertising,ProductLine,Salesforce,Pricing,Warranty,NewProduct,Ordering,Flexibility,Dilivery)~Region,data=now)
summary(now.mod)
Manova(now.mod,multivarite=TRUE,type=c("II"),test=("Wilks"))
