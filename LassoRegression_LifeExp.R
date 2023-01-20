#loading the data
ldata <- read.csv("C://Users/yshai/Downloads/DMML Datsets/Life Expectancy Data.csv", header = TRUE)
str(ldata)
summary(ldata)
#no of rows
nrow(ldata)



#check null values
anyNA(ldata)
sum(is.null(ldata))
data.frame(colSums(is.na(ldata)))
#Drop null values
ldata<-na.omit(ldata)



#Correlation
library(corrplot)



corContVar <-cor(ldata[sapply(ldata,is.numeric)],use = "complete.obs")
col <- colorRampPalette(c("darkgoldenrod4", "burlywood1",
                          "darkkhaki", "darkgreen"))
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


#Applying Lasso Regression

#define response variable
y<-ldata$Life.expectancy
#define matrix of predictor variables
x <- data.matrix(ldata[, c('Year', 'Life.expectancy','Adult.Mortality','infant.deaths','Measles','BMI','Polio','Total.expenditure','Alcohol')])
#install package
#install.packages('glmnet')
library(glmnet)



#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)



#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda



#produce plot of test MSE by lambda value
plot(cv_model)



#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)



#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)



#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)



#find R-Squared
rsq <- 1 - sse/sst
rsq

#plotting
plot(cv_model,xvar="lambda")

