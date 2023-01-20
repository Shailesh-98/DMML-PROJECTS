install.packages("Amelia")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("stats")
install.packages("lubridate")
install.packages("corrplot")
install.packages("caTools")
install.packages("adabag")
install.packages("caret")




library(Amelia)
library(tidyverse)
library(dplyr)
library(stats)
library(lubridate)
library(corrplot)
library(caTools)
library(adabag)
library(caret)

heart <- read.csv("C://Users/yshai/Downloads/DMML Datsets/heart.csv", header = TRUE)
str(heart)
#Loading the data
#since our data is too huge , so we are taking only 10000 rows into consideration
heart <- heart[1:10000,]


#checking the number of rows of data

nrow(heart)



#checking the null values in the data
anyNA(heart)
summary(heart)
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

histogram(heart$BMI)

#Correlation Matrix 
library(corrplot)
corContVar <-cor(heart[sapply(heart,is.numeric)],use = "complete.obs")
col <- colorRampPalette(c("darkgoldenrod4", "red",
                          "darkblue", "darkgreen"))
corrplot(corContVar,method = "number",col=col(200),
         order="hclust",
         type = "full",
         #addCoef.col = "black",
         tl.col="black", tl.srt=45, tl.cex=0.7, tl.offset = 0.5,
         number.cex=0.5,
         #diag = FALSE,
         number.digits = 2)
corrplot(corContVar, method="color", col=col(200),
         order="hclust",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, tl.offset = 0.5, #Text label color and rotation
)
mtext("Correlation Plot", family = "serif",
      col = "#0B3948", side = 2, at= 5, line= 2.50, cex=2)


#splitting the data into training and test data
id <- sample(2,nrow(heart), prob = c(0.7,0.3), replace = T)
heart_ab_train <- heart[id==1,]
heart_ab_test <- heart[id==2,]



train = heart[parts, ]
test = heart[-parts, ]



nrow(train)
nrow(test)



#applying the ada boost model
# train a model using our training data
model_adaboost <- boosting(HeartDisease~., data=heart_ab_train , boos=TRUE)



#taking the summary of ada boost model created
summary(model_adaboost)



#using model to make predictions on test data
pred_test = predict(model_adaboost, heart_ab_test)



# Returns the prediction values of test data along with the confusion matrix
pred_test



#Checking the accuracy
accuracy_model <- (2626+52)/(2626+52+267+64)
accuracy_model



#Taking the precision
taking_precision <- (52)/(52+267)
taking_precision



#taking recall
taking_recall <- (52)/(52+64)
taking_recall



#taking F1 score
F1_SCORE <- (2*taking_precision*taking_recall)/(taking_precision+taking_recall)
F1_SCORE



colnames(heart)
importanceplot(model_adaboost)



model_adaboost <- boosting(HeartDisease~BMI+PhysicalHealth+MentalHealth+AgeCategory+SleepTime+GenHealth+Sex, data=heart_ab_train , boos=TRUE)



summary(model_adaboost)



pred_test = predict(model_adaboost, heart_ab_test)



pred_test



accuracy_model <- (2650+27)/(2650+27+292+40)
accuracy_model



taking_precision <- (27)/(27+292)
taking_precision



taking_recall <- (27)/(27+292)
taking_recall



F1_SCORE <- (2*taking_precision*taking_recall)/(taking_precision+taking_recall)
F1_SCORE



ggplot(heart, aes(x=HeartDisease)) + geom_bar()
missmap(heart)



