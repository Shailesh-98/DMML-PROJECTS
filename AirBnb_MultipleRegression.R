Airbnb <- read.csv("C://Users/yshai/Downloads/DMML Datsets/UK_AirBnb.csv")
str(Airbnb)
summary(Airbnb)
library(caTools)
split <- sample.split(Airbnb, SplitRatio=0.8)
split
train<-subset(Airbnb,split='TRUE')
test<-subset(Airbnb,split='FALSE')
#Model Building

model <- lm(price~ minimum_nights, data=train)
summary(model)
