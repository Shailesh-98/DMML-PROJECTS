life <- read.csv("C://Users/yshai/Downloads/DMML Datsets/UK_AirBnb.csv",header = TRUE)
str(life)
summary(life)



#no of rows
nrow(life)



#check null values
anyNA(life)
sum(is.null(life))
data.frame(colSums(is.na(life)))
#Drop null values
life<-na.omit(life)



#Correlation
library(corrplot)



corContVar <-cor(life[sapply(life,is.numeric)],use = "complete.obs")
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



#define response variable
y <- life$price



#define matrix of predictor variables
x <- data.matrix(life[, c('latitude','minimum_nights','number_of_reviews','availability_365','reviews_per_month','calculated_host_listings_count')])



library(glmnet)



#fit ridge regression model
model <- glmnet(x, y, alpha = 0)



#view summary of model
summary(model)



#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 0)



#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda




#produce plot of test MSE by lambda value
plot(cv_model)



#find coefficients of best model
best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model)



#produce Ridge trace plot
plot(model, xvar = "lambda")



#use fitted best model to make predictions
y_predicted <- predict(model, s = best_lambda, newx = x)



#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)



#find R-Squared
rsq <- 1 - sse/sst
rsq