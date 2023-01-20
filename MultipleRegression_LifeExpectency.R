#Loading the data
life <- read.csv("C://Users/yshai/Downloads/DMML Datsets/Life Expectancy Data.csv", header = TRUE)
str(life)
summary(life)



#Data cleaning
data.frame(colSums(is.na(life)))
anyNA(life)
#Drop null values
life<-na.omit(life)



#splitting the data into training and test
#install.packages('caTools')
library(caTools)



#indexing our data to further divide it into training and testing data 
parts = createDataPartition(life$Life.expectancy, p = 0.8, list = F)





#splitting the data into training and test data
id <- sample(2,nrow(life), prob = c(0.7,0.3), replace = T)
life_ab_train <- life[id==1,]
life_ab_test <- life[id==2,]




train = life[parts, ]
test = life[-parts, ]



#create the model
model<-lm(Life.expectancy~Year+Alcohol+BMI+Adult.Mortality+percentage.expenditure+Population+GDP+Income.composition.of.resources,data=train)
summary(model)



#prediction
pred<-predict(model,test)
summary(pred)

#comparing predicted vs Actual value
plot(pred,lty=1.8,col="blue")
plot(test$Life.expectancy,lty=1.8,col="blue")
lines(pred,col='red')



#finding Accuracy
rmse<-sqrt(mean(pred~life$Life.expectancy)^2)
rmse
