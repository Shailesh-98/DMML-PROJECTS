
install.packages("Amelia")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("stats")
install.packages("lubridate")
install.packages("corrplot")
install.packages("caTools")
install.packages("e1071")
install.packages("caret")
installed.packages("klar")
install.packages("gains")
install.packages("pROC")



#loading the required library
library(e1071)
library(caret)
library(klar)
library(gains)
library(pROC)
library(Amelia)



#Loading the data
heart <- read.csv("C://Users/yshai/Downloads/DMML Datsets/heart.csv", header = TRUE)
nrow(heart)


#since our data is too huge , so we are taking only 10000 rows into consideration
heart <- heart[1:10000,]



#checking the number of rows of data
nrow(heart)



#checking the null values in the data
anyNA(heart)



str(heart)
#summary of data
summary(heart)



head(heart)



#as the missing values are zero
missmap(heart)

heart$HeartDisease<- as.factor(heart$HeartDisease)
str(heart)
#indexing our data to further divide it into training and testing data 
parts = createDataPartition(heart$HeartDisease, p = 0.8, list = F)




#splitting the data into training and test data
id <- sample(2,nrow(heart), prob = c(0.7,0.3), replace = T)
heart_ab_train <- heart[id==1,]
heart_ab_test <- heart[id==2,]



train = heart[parts, ]
test = heart[-parts, ]



nrow(train)
nrow(test)



#aApplying Naive Bayes 
length(heart$HeartDisease)
nrow(heart_ab_test)
model_nB <- naiveBayes(heart$HeartDisease~.,heart)



summary(model_nB)



#using model to make predictions on test data
pred_test = predict(model_nB, heart_ab_test)
pred_test



#confusion matrix
cm <- table(heart_ab_test$HeartDisease, pred_test)
cm



#Checking the accuracy
accuracy_model <- (2488+140)/(2488+140+276+178)
accuracy_model



#Taking the precision
taking_precision <- (140)/(140+276)
taking_precision



#taking recall
taking_recall <- (140)/(140+178)
taking_recall



#taking F1 score
F1_SCORE <- (2*taking_precision*taking_recall)/(taking_precision+taking_recall)
F1_SCORE




#Featureselection
model_nB <- naiveBayes(HeartDisease~BMI+PhysicalHealth+MentalHealth+AgeCategory+SleepTime+GenHealth+Sex, data=heart_ab_train)
summary(model_nB)



#using model to make predictions on test data
pred_test = predict(model_nB, heart_ab_test)
pred_test



#confusion matrix
cm <- table(heart_ab_test$HeartDisease, pred_test)
cm



#Checking the accuracy
accuracy_model <- (2555+98)/(2555+98+209+220)
accuracy_model



#Taking the precision
taking_precision <- (98)/(98+209)
taking_precision



#taking recall
taking_recall <- (98)/(98+220)
taking_recall


histogram(heart$HeartDisease)

#taking F1 score
F1_SCORE <- (2*taking_precision*taking_recall)/(taking_precision+taking_recall)
F1_SCORE
